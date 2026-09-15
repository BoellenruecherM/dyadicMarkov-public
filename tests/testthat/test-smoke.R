# Add dimnames from `src` to `dst` (recursively for lists), only when dst lacks them
.add_dimnames_from <- function(dst, src) {
  if ((is.matrix(dst) || is.array(dst)) && (is.matrix(src) || is.array(src))) {
    if (is.null(dimnames(dst)) && !is.null(dimnames(src))) {
      dimnames(dst) <- dimnames(src)
    }
    return(dst)
  }
  if (is.list(dst) && is.list(src)) {
    n <- min(length(dst), length(src))
    for (i in seq_len(n)) dst[[i]] <- .add_dimnames_from(dst[[i]], src[[i]])
    return(dst)
  }
  dst
}


# internal helper: fill theoretical matrix by groups of row indices
fill_theo_by_groups__local <- function(empirical, groups) {
  out <- matrix(0, nrow = nrow(empirical), ncol = ncol(empirical))

  for (idx in groups) {
    denom <- sum(empirical[idx, , drop = FALSE])
    if (denom == 0) {
      p <- 0
    } else {
      p <- sum(empirical[idx, 1]) / denom
      if (is.nan(p) || is.infinite(p)) p <- 0
    }

    rs <- rowSums(empirical[idx, , drop = FALSE])
    out[idx, ] <- matrix(c(p, 1 - p),
                         nrow = length(idx), ncol = ncol(empirical), byrow = TRUE
    ) * rs
  }

  out
}


fill_many_theo_local <- function(empirical, group_sets){
  gamma  <- rowSums(empirical)
  states <- ncol(empirical)

  fill_one <- function(groups){
    out <- matrix(0, nrow = nrow(empirical), ncol = states)

    for (g in groups) {
      block <- empirical[g, , drop = FALSE]
      denom <- sum(block)

      p <- if (denom == 0) rep(0, states) else colSums(block) / denom

      # each row i in g: out[i, ] = p * gamma[i]
      Pmat <- matrix(rep(p, each = length(g)), nrow = length(g), ncol = states)
      out[g, ] <- Pmat * gamma[g]
    }

    out
  }

  lapply(group_sets, fill_one)
}


test_that("exported API exists", {
  ex <- getNamespaceExports("dyadicMarkov")
  expect_true(all(c(
    "univariatePattern","countEmpBivariate","bivariateCase","partialPattern","completePattern"
  ) %in% ex))
})

test_that("extdata chap6 folder exists", {
  p <- system.file("extdata", "chap6", package = "dyadicMarkov")
  skip_if_not(nzchar(p) && dir.exists(p), "extdata not shipped in CRAN build")
  expect_true(nzchar(p))
  expect_true(dir.exists(p))
})


test_that("univariatePattern runs deterministically (seed 888)", {
  set.seed(888)
  states <- 2
  chainFM <- c(1,1,2,2,1,2,1,1,2,2)
  chainSM <- c(1,2,2,1,1,1,2,2,1,2)

  res <- dyadicMarkov::univariatePattern(chainFM, chainSM, states = states, alpha = 0.05)

  expect_true(is.list(res))
  expect_true(all(c("TEST.AM","TEST.PM","pattern") %in% names(res)))
  expect_s3_class(res$TEST.AM, "htest")
  expect_s3_class(res$TEST.PM, "htest")
  expect_true(res$pattern %in% c("IM (A0)","PM (A3)","AM (A2)","APM (A1)"))
})

test_that("refactor helper produces valid theo matrices (finite + same dims)", {
  empirical <- matrix(c(
    10,0,  8,2,  6,4,  4,6,
    2,8,  0,10, 5,5,  3,7,
    7,3,  9,1,  2,8,  4,6,
    6,4,  1,9,  8,2,  0,10
  ), ncol = 2, byrow = TRUE)

  d <- dyadicMarkov:::countTheoBivariateC3(empirical)
  e <- dyadicMarkov:::countTheoBivariateC2(empirical)

  expect_length(d, 4)
  expect_length(e, 4)
  expect_true(all(vapply(d, function(m) all(dim(m) == dim(empirical)), logical(1))))
  expect_true(all(vapply(e, function(m) all(dim(m) == dim(empirical)), logical(1))))
  expect_true(all(is.finite(unlist(d))))
  expect_true(all(is.finite(unlist(e))))
})

test_that("countTheo matches legacy implementation (AM/PM)", {
  countTheo_old <- function(empirical, pattern = c("AM","PM")){
    gamma <- rowSums(empirical)
    states <- ncol(empirical)
    xi <- vector()
    int <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))
    neta <- matrix(NA, nrow=ncol(empirical), ncol=ncol(empirical))
    countTheo <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

    if(pattern == "AM"){
      sumGroup <- (diag(rep(1, ncol(empirical))) %x% t(rep(1, ncol(empirical))) %*% empirical)
      for(i in 1:states){
        xiValue <- sum(gamma[(1+(i-1)*states):(i*states)])
        xi <- append(xi, xiValue)
      }
      for(i in 1:length(xi)){
        neta[i,] <- sumGroup[i,]/xi[i]
      }
      for(i in 1:ncol(neta)){
        for(j in 1:ncol(neta)){
          int[(j+states*(i-1)),] <- neta[i,]
        }
      }
      for(i in 1:nrow(empirical)){
        countTheo[i,] <- (int[i,]*gamma[i])
      }
    }
    if(pattern == "PM"){
      sumGroup <- ((do.call(cbind, replicate(states, diag(rep(1, ncol(empirical))), simplify = FALSE))) %*% empirical)
      for(i in 1:states){
        num <- vector()
        for(j in 1:states){
          pos <- gamma[i + states*(j-1)]
          num <- append(num, pos)
        }
        xiValue <- sum(num)
        xi <- append(xi, xiValue)
      }
      for(i in 1:length(xi)){
        neta[i,] <- sumGroup[i,]/xi[i]
      }
      int <- do.call(rbind, replicate(states, neta, simplify=FALSE))
      for(i in 1:nrow(empirical)){
        countTheo[i,] <- (int[i,]*gamma[i])
      }
    }

    countTheo[is.nan(countTheo)] <- 0
    return(countTheo)
  }

  set.seed(888)
  states <- 3
  empirical <- matrix(sample(0:10, states^2 * states, replace = TRUE),
                      nrow = states^2, ncol = states)

  expect_equal(dyadicMarkov:::countTheo(empirical, "AM"), countTheo_old(empirical, "AM"))
  expect_equal(dyadicMarkov:::countTheo(empirical, "PM"), countTheo_old(empirical, "PM"))
})

test_that("countEmp matches legacy implementation", {
  countEmp_old <- function(states, chainFM, chainSM){
    chainCount <- (length(chainFM)-1)
    count <- matrix(0, nrow = states*states, ncol = states)
    for(i in 1:chainCount){
      column <- chainFM[i+1]
      behaviorFM <- chainFM[i]
      behaviorSM <- chainSM[i]
      row <- ((1+states*(behaviorFM-1))+(behaviorSM-1))
      count[row, column] <- (count[row, column]+1)
    }
    if(sum(count)!=(length(chainFM)-1)){
      stop("Error in the count")
    }
    count
  }

  set.seed(888)
  states <- 3
  T <- 50
  chainFM <- sample.int(states, T, replace = TRUE)
  chainSM <- sample.int(states, T, replace = TRUE)

  expect_equal(
    unname(unclass(dyadicMarkov:::countEmp(chainFM, chainSM, states = states))),
    countEmp_old(states, chainFM, chainSM)
  )
})

test_that("countEmpBivariate matches legacy implementation (states=2)", {
  countEmpBivariate_old <- function(states, chainFM_V1, chainSM_V1, chainFM_V2, chainSM_V2){
    if(states != 2) stop("legacy bivariate code assumes states=2 in current implementation")
    if((length(chainFM_V1) != length(chainFM_V2))
       || (length(chainSM_V1) != length(chainSM_V2))
       || (length(chainFM_V1) != length(chainSM_V1))){
      stop("Error, the chains are of different lengths")
    }
    chainCount <- (length(chainFM_V1)-1)
    count <- matrix(0, nrow = 4*states*states, ncol = states)
    for(i in 1:chainCount){
      column <- chainFM_V1[i+1]
      behaviorFM_V1 <- chainFM_V1[i]
      behaviorSM_V1 <- chainSM_V1[i]
      behaviorFM_V2 <- chainFM_V2[i]
      behaviorSM_V2 <- chainSM_V2[i]
      row <- states^2*(states*(behaviorFM_V1-1)+(behaviorSM_V1-1)) + states*(behaviorFM_V2-1)+(behaviorSM_V2-1) + 1
      count[row, column] <- (count[row, column]+1)
    }
    count
  }

  set.seed(888)
  states <- 2
  T <- 60
  chainFM_V1 <- sample.int(states, T, replace = TRUE)
  chainSM_V1 <- sample.int(states, T, replace = TRUE)
  chainFM_V2 <- sample.int(states, T, replace = TRUE)
  chainSM_V2 <- sample.int(states, T, replace = TRUE)

  got <- dyadicMarkov:::countEmpBivariate(
    chainFM_V1,
    chainSM_V1,
    chainFM_V2,
    chainSM_V2,
    states = states
  )

  got_values <- unclass(got)

  attr(
    got_values,
    "dyadic_sequences"
  ) <- NULL

  expect_equal(
    unname(got_values),
    countEmpBivariate_old(
      states,
      chainFM_V1,
      chainSM_V1,
      chainFM_V2,
      chainSM_V2
    )
  )
})

test_that("chisquaredDist matches legacy implementation", {
  chisquaredDist_old <- function(population, empirical){
    chidist <- 0
    for(i in 1:nrow(population)){
      for(j in 1:ncol(population)){
        numerator <- ((empirical[i,j]-population[i,j])^2)
        denominator <- population[i,j]
        if(isTRUE(denominator == 0)){
          ratio <- 0
        } else{
          ratio <- (numerator/denominator)
        }
        chidist <- (chidist+ratio)
      }
    }
    chidist
  }

  set.seed(888)
  population <- matrix(sample(c(0,1:10), 30, replace = TRUE), nrow = 10, ncol = 3)
  empirical  <- matrix(sample(0:10, 30, replace = TRUE), nrow = 10, ncol = 3)

  expect_equal(
    dyadicMarkov:::.chisquaredDist(population, empirical),
    chisquaredDist_old(population, empirical),
    tolerance = 1e-12
  )
})

test_that("lrtLocal matches legacy implementation (statistic + p.value)", {
  lrtLocal_old <- function(population, empirical){
    method      <- "Chi-squared test"
    dataName   <- "Observed vs Estimated"
    alternative <- "The unrestricted model fits the data better"
    khi2 <- chisquaredDist_old(population = population, empirical = empirical)
    degree <- (ncol(population)*(ncol(population)-1)^2)
    pValue <- stats::pchisq(q=khi2, df=degree, lower.tail = FALSE)
    names(khi2) <- "X-squared"
    names(degree) <- "df"
    TEST <- list(method = method, data.name = dataName,
                 parameter = degree, alternative = alternative,
                 statistic = khi2, p.value = pValue)
    class(TEST) <- "htest"
    TEST
  }

  chisquaredDist_old <- function(population, empirical){
    chidist <- 0
    for(i in 1:nrow(population)){
      for(j in 1:ncol(population)){
        numerator <- ((empirical[i,j]-population[i,j])^2)
        denominator <- population[i,j]
        if(isTRUE(denominator == 0)){
          ratio <- 0
        } else{
          ratio <- (numerator/denominator)
        }
        chidist <- (chidist+ratio)
      }
    }
    chidist
  }

  set.seed(888)
  population <- matrix(sample(c(0,1:10), 24, replace = TRUE), nrow = 8, ncol = 3)
  empirical  <- matrix(sample(0:10, 24, replace = TRUE), nrow = 8, ncol = 3)

  new <- dyadicMarkov:::lrtLocal(population, empirical)
  old <- lrtLocal_old(population, empirical)

  expect_equal(as.numeric(new$statistic), as.numeric(old$statistic), tolerance = 1e-12)
  expect_equal(as.numeric(new$p.value),    as.numeric(old$p.value),    tolerance = 1e-12)
  expect_equal(as.numeric(new$parameter),  as.numeric(old$parameter))
})

test_that("mleEstimation matches legacy implementation", {
  mleEstimation_old <- function(empirical){
    estimate <- matrix(0, nrow = nrow(empirical), ncol = ncol(empirical))
    rowSum <- rowSums(empirical)
    for(i in 1:nrow(empirical)){
      if(isTRUE(rowSum[i]==0)){
        estimate[i,] <- (1/ncol(empirical))
      } else {
        vectorProb <- (empirical[i,]/rowSum[i])
        estimate[i,] <- vectorProb
      }
    }
    estimate
  }

  set.seed(888)
  empirical <- matrix(sample(0:10, 40, replace = TRUE), nrow = 10, ncol = 4)

  expect_equal(
    unname(unclass(dyadicMarkov:::mleEstimation(empirical))),
    mleEstimation_old(empirical)
  )
})

test_that("aicBivariate matches legacy implementation", {
  aicBivariate_old <- function(population, empirical, test = c("single","duo", "triplet", "quadruplet")){
    if(isTRUE(test=="single")) k <- 2
    if(isTRUE(test=="duo")) k <- 4
    if(isTRUE(test=="triplet")) k <- 8
    if(isTRUE(test=="quadruplet")) k <- 16
    G2 <- 2*sum(empirical * log(empirical/population), na.rm = TRUE)
    2*k + G2
  }

  set.seed(888)
  population <- matrix(sample(c(0,1:10), 60, replace = TRUE), nrow = 15, ncol = 4)
  empirical  <- matrix(sample(0:10, 60, replace = TRUE), nrow = 15, ncol = 4)

  for (tst in c("single","duo","triplet","quadruplet")) {
    expect_equal(
      dyadicMarkov:::aicBivariate(population, empirical, tst),
      aicBivariate_old(population, empirical, tst),
      tolerance = 1e-12
    )
  }
})

test_that("lrtLocal and bivariateTest retain numeric fields and identify Pearson", {
  lrtLocal_old <- function(population, empirical){
    method      <- "Chi-squared test"
    dataName   <- "Observed vs Estimated"
    alternative <- "The unrestricted model fits the data better"
    khi2 <- dyadicMarkov:::.chisquaredDist(population = population, empirical = empirical)
    degree <- (ncol(population)*(ncol(population)-1)^2)
    pValue <- stats::pchisq(q=khi2, df=degree, lower.tail = FALSE)
    names(khi2) <- "X-squared"
    names(degree) <- "df"
    TEST <- list(method = method, data.name = dataName,
                 parameter = degree, alternative = alternative,
                 statistic = khi2, p.value = pValue)
    class(TEST) <- "htest"
    TEST
  }

  bivariateTest_old <- function(population, empirical){
    df <- 12
    method      <- "Chi-squared test"
    dataName   <- "Observed vs Estimated"
    alternative <- "The unrestricted model fits the data better"
    khi2 <- dyadicMarkov:::.chisquaredDist(population = population, empirical = empirical)
    degree <- df
    pValue <- stats::pchisq(q=khi2, df=degree, lower.tail = FALSE)
    names(khi2) <- "X-squared"
    names(degree) <- "df"
    TEST <- list(method = method, data.name = dataName,
                 parameter = degree, alternative = alternative,
                 statistic = khi2, p.value = pValue)
    class(TEST) <- "htest"
    TEST
  }

  set.seed(888)
  population <- matrix(sample(c(0,1:10), 24, replace = TRUE), nrow = 8, ncol = 3)
  empirical  <- matrix(sample(0:10, 24, replace = TRUE), nrow = 8, ncol = 3)

  new1 <- dyadicMarkov:::lrtLocal(population, empirical)
  old1 <- lrtLocal_old(population, empirical)
  expect_identical(new1$method, "Pearson's chi-squared test")
  expect_equal(new1$data.name, old1$data.name)
  expect_equal(new1$alternative, old1$alternative)
  expect_equal(as.numeric(new1$parameter), as.numeric(old1$parameter))
  expect_equal(as.numeric(new1$statistic), as.numeric(old1$statistic), tolerance = 1e-12)
  expect_equal(as.numeric(new1$p.value), as.numeric(old1$p.value), tolerance = 1e-12)

  new2 <- dyadicMarkov:::bivariateTest(population, empirical)
  old2 <- bivariateTest_old(population, empirical)
  expect_identical(new2$method, "Chi-squared test")
  expect_equal(new2$data.name, old2$data.name)
  expect_equal(new2$alternative, old2$alternative)
  expect_equal(as.numeric(new2$parameter), as.numeric(old2$parameter))
  expect_equal(as.numeric(new2$statistic), as.numeric(old2$statistic), tolerance = 1e-12)
  expect_equal(as.numeric(new2$p.value), as.numeric(old2$p.value), tolerance = 1e-12)
})

test_that("countTheoBivariateG/P match legacy implementations", {

  countTheoBivariateG_old <- function(empirical){
    blocA1.1 <- empirical[1:4,]
    blocA1.2 <- empirical[5:8,]
    blocA1.3 <- empirical[9:12,]
    blocA1.4 <- empirical[13:16,]

    blocB1.1 <- empirical[c(1,5,9,13),]
    blocB1.2 <- empirical[c(2,6,10,14),]
    blocB1.3 <- empirical[c(3,7,11,15),]
    blocB1.4 <- empirical[c(4,8,12,16),]

    theoA1 <- rbind(
      matrix(c(rep((sum(blocA1.1[,1])/sum(blocA1.1)),4), rep((1-(sum(blocA1.1[,1])/sum(blocA1.1))),4)), byrow=FALSE, ncol=2)*rowSums(blocA1.1),
      matrix(c(rep((sum(blocA1.2[,1])/sum(blocA1.2)),4), rep((1-(sum(blocA1.2[,1])/sum(blocA1.2))),4)), byrow=FALSE, ncol=2)*rowSums(blocA1.2),
      matrix(c(rep((sum(blocA1.3[,1])/sum(blocA1.3)),4), rep((1-(sum(blocA1.3[,1])/sum(blocA1.3))),4)), byrow=FALSE, ncol=2)*rowSums(blocA1.3),
      matrix(c(rep((sum(blocA1.4[,1])/sum(blocA1.4)),4), rep((1-(sum(blocA1.4[,1])/sum(blocA1.4))),4)), byrow=FALSE, ncol=2)*rowSums(blocA1.4)
    )

    theoB1 <- matrix(NA, ncol=ncol(empirical), nrow=nrow(empirical))
    theoB1[c(1,5,9,13),] <- matrix(c(rep((sum(blocB1.1[,1])/sum(blocB1.1)),4), rep((1-(sum(blocB1.1[,1])/sum(blocB1.1))),4)), byrow=FALSE, ncol=2)*rowSums(blocB1.1)
    theoB1[c(2,6,10,14),] <- matrix(c(rep((sum(blocB1.2[,1])/sum(blocB1.2)),4), rep((1-(sum(blocB1.2[,1])/sum(blocB1.2))),4)), byrow=FALSE, ncol=2)*rowSums(blocB1.2)
    theoB1[c(3,7,11,15),] <- matrix(c(rep((sum(blocB1.3[,1])/sum(blocB1.3)),4), rep((1-(sum(blocB1.3[,1])/sum(blocB1.3))),4)), byrow=FALSE, ncol=2)*rowSums(blocB1.3)
    theoB1[c(4,8,12,16),] <- matrix(c(rep((sum(blocB1.4[,1])/sum(blocB1.4)),4), rep((1-(sum(blocB1.4[,1])/sum(blocB1.4))),4)), byrow=FALSE, ncol=2)*rowSums(blocB1.4)

    theoA1[is.nan(theoA1)] <- 0
    theoB1[is.nan(theoB1)] <- 0

    list(theoA1, theoB1)
  }

  countTheoBivariateP_old <- function(empirical){
    blocB2.1 <- empirical[c(1,2,5,6,9,10,13,14),]
    blocB2.2 <- empirical[c(3,4,7,8,11,12,15,16),]

    blocB3.1 <- empirical[c(1,3,5,7,9,11,13,15),]
    blocB3.2 <- empirical[c(2,4,6,8,10,12,14,16),]

    theoB2 <- matrix(NA, ncol=ncol(empirical), nrow=nrow(empirical))
    theoB2[c(1,2,5,6,9,10,13,14),] <- matrix(c(rep((sum(blocB2.1[,1])/sum(blocB2.1)),8), rep((1-(sum(blocB2.1[,1])/sum(blocB2.1))),8)), byrow=FALSE, ncol=2)*rowSums(blocB2.1)
    theoB2[c(3,4,7,8,11,12,15,16),] <- matrix(c(rep((sum(blocB2.2[,1])/sum(blocB2.2)),8), rep((1-(sum(blocB2.2[,1])/sum(blocB2.2))),8)), byrow=FALSE, ncol=2)*rowSums(blocB2.2)

    theoB3 <- matrix(NA, ncol=ncol(empirical), nrow=nrow(empirical))
    theoB3[c(1,3,5,7,9,11,13,15),] <- matrix(c(rep((sum(blocB3.1[,1])/sum(blocB3.1)),8), rep((1-(sum(blocB3.1[,1])/sum(blocB3.1))),8)), byrow=FALSE, ncol=2)*rowSums(blocB3.1)
    theoB3[c(2,4,6,8,10,12,14,16),] <- matrix(c(rep((sum(blocB3.2[,1])/sum(blocB3.2)),8), rep((1-(sum(blocB3.2[,1])/sum(blocB3.2))),8)), byrow=FALSE, ncol=2)*rowSums(blocB3.2)

    theoB2[is.nan(theoB2)] <- 0
    theoB3[is.nan(theoB3)] <- 0

    list(theoB2, theoB3)
  }

  set.seed(888)
  empirical <- matrix(sample(0:10, 32, replace = TRUE), nrow = 16, ncol = 2)

  actual_g   <- dyadicMarkov:::countTheoBivariateG(empirical)
  expected_g <- countTheoBivariateG_old(empirical)
  expected_g <- .add_dimnames_from(expected_g, actual_g)
  expect_equal(actual_g, expected_g)

  actual_p   <- dyadicMarkov:::countTheoBivariateP(empirical)
  expected_p <- countTheoBivariateP_old(empirical)
  expected_p <- .add_dimnames_from(expected_p, actual_p)
  expect_equal(actual_p, expected_p)
})

test_that("partialPattern/completePattern unchanged after caching theo matrices", {

  partialPattern_old <- function(empirical){
    countTheoB1 <- dyadicMarkov:::countTheoBivariateG(empirical)[[2]]
    countTheoB2 <- dyadicMarkov:::countTheoBivariateP(empirical)[[1]]
    countTheoB3 <- dyadicMarkov:::countTheoBivariateP(empirical)[[2]]

    b1 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoB1, test = "duo")
    b2 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoB2, test = "single")
    b3 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoB3, test = "single")

    aicMat <- matrix(
      c("partial actor-partner (B1)", "B1", b1,
        "partial actor only (B2)", "B2", b2,
        "partial partner only (B3)", "B3", b3),
      ncol = 3, byrow = TRUE
    )
    aicMat <- as.data.frame(aicMat)
    colnames(aicMat) <- c("pattern", "matrix","aic")
    aicMat$aic <- as.numeric(aicMat$aic)

    aicVec <- list("partial actor-partner (B1)" = b1, "partial actor only (B2)" = b2, "partial partner only (B3)" = b3)
    type <- names(aicVec)[which.min(aicVec)]
    list(aic = aicMat, pattern = type)
  }

  completePattern_old <- function(empirical){
    countTheoD1 <- dyadicMarkov:::countTheoBivariateC3(empirical)[[1]]
    countTheoD2 <- dyadicMarkov:::countTheoBivariateC3(empirical)[[2]]
    countTheoD3 <- dyadicMarkov:::countTheoBivariateC3(empirical)[[3]]
    countTheoD4 <- dyadicMarkov:::countTheoBivariateC3(empirical)[[4]]

    countTheoE1 <- dyadicMarkov:::countTheoBivariateC2(empirical)[[1]]
    countTheoE2 <- dyadicMarkov:::countTheoBivariateC2(empirical)[[2]]
    countTheoE3 <- dyadicMarkov:::countTheoBivariateC2(empirical)[[3]]
    countTheoE4 <- dyadicMarkov:::countTheoBivariateC2(empirical)[[4]]

    c <- dyadicMarkov:::aicBivariate(empirical = empirical, population = empirical, test = "quadruplet")

    d1 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoD1, test = "triplet")
    d2 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoD2, test = "triplet")
    d3 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoD3, test = "triplet")
    d4 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoD4, test = "triplet")

    e1 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoE1, test = "duo")
    e2 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoE2, test = "duo")
    e3 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoE3, test = "duo")
    e4 <- dyadicMarkov:::aicBivariate(empirical = empirical, population = countTheoE4, test = "duo")

    aicMat <- matrix(
      c("complete actor-partner (C)", "C", c,
        "partner only on the main, actor-partner on the second (D1)", "D1", d1,
        "actor only on the main, actor-partner on the second (D2)", "D2", d2,
        "actor-partner on the main, partner only on the second (D3)", "D3", d3,
        "actor-partner on the main, actor only on the second (D4)", "D4", d4,
        "complete partner only (E1)", "E1", e1,
        "partner only on the main, actor only on the second (E2)", "E2", e2,
        "actor only on the main, partner only on the second (E3)", "E3", e3,
        "complete actor only (E4)", "E4", e4),
      ncol = 3, byrow = TRUE
    )

    aicMat <- as.data.frame(aicMat)
    colnames(aicMat) <- c("pattern", "matrix","aic")
    aicMat$aic <- as.numeric(aicMat$aic)

    aicVec <- list(
      "complete actor-partner (C)" = as.numeric(c),
      "partner only on the main, actor-partner on the second (D1)" = as.numeric(d1),
      "actor only on the main, actor-partner on the second (D2)" = as.numeric(d2),
      "actor-partner on the main, partner only on the second (D3)" = as.numeric(d3),
      "actor-partner on the main, actor only on the second (D4)" = as.numeric(d4),
      "complete partner only (E1)" = as.numeric(e1),
      "partner only on the main, actor only on the second (E2)" = as.numeric(e2),
      "actor only on the main, partner only on the second (E3)" = as.numeric(e3),
      "complete actor only (E4)" = as.numeric(e4)
    )

    type <- names(aicVec)[which.min(aicVec)]
    list(aic = aicMat, pattern = type)
  }

  set.seed(888)
  empirical <- matrix(sample(0:10, 32, replace = TRUE), nrow = 16, ncol = 2)

  actual_partial <- dyadicMarkov:::partialPattern(empirical)
  expected_partial <- partialPattern_old(empirical)
  expect_equal(actual_partial$aic, expected_partial$aic)
  expect_equal(actual_partial$pattern, expected_partial$pattern)

  actual_complete <- dyadicMarkov:::completePattern(empirical)
  expected_complete <- completePattern_old(empirical)
  expect_equal(actual_complete$aic, expected_complete$aic)
  expect_equal(actual_complete$pattern, expected_complete$pattern)
})

test_that("countTheoBivariateC2/C3 unchanged after .fill_many_theo refactor", {

  countTheoBivariateC3_old <- function(empirical){
    groups_D1 <- list(c(1,9), c(2,10), c(3,11), c(4,12), c(5,13), c(6,14), c(7,15), c(8,16))
    groups_D2 <- list(c(1,5), c(2,6), c(3,7), c(4,8), c(9,13), c(10,14), c(11,15), c(12,16))
    groups_D3 <- list(c(1,3), c(2,4), c(5,7), c(6,8), c(9,11), c(10,12), c(13,15), c(14,16))
    groups_D4 <- list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14), c(15,16))

    theoD1 <- fill_theo_by_groups__local(empirical, groups_D1)
    theoD2 <- fill_theo_by_groups__local(empirical, groups_D2)
    theoD3 <- fill_theo_by_groups__local(empirical, groups_D3)
    theoD4 <- fill_theo_by_groups__local(empirical, groups_D4)

    list(theoD1, theoD2, theoD3, theoD4)
  }

  countTheoBivariateC2_old <- function(empirical){
    groups_E1 <- list(c(1,3,9,11), c(2,4,10,12), c(5,7,13,15), c(6,8,14,16))
    groups_E2 <- list(c(1,2,9,10), c(3,4,11,12), c(5,6,13,14), c(7,8,15,16))
    groups_E3 <- list(c(1,3,5,7), c(2,4,6,8), c(9,11,13,15), c(10,12,14,16))
    groups_E4 <- list(c(1,2,5,6), c(3,4,7,8), c(9,10,13,14), c(11,12,15,16))

    theoE1 <- fill_theo_by_groups__local(empirical, groups_E1)
    theoE2 <- fill_theo_by_groups__local(empirical, groups_E2)
    theoE3 <- fill_theo_by_groups__local(empirical, groups_E3)
    theoE4 <- fill_theo_by_groups__local(empirical, groups_E4)

    list(theoE1, theoE2, theoE3, theoE4)
  }

  set.seed(888)
  empirical <- matrix(sample(0:10, 32, replace = TRUE), nrow = 16, ncol = 2)

  actual_c3   <- dyadicMarkov:::countTheoBivariateC3(empirical)
  expected_c3 <- countTheoBivariateC3_old(empirical)
  expected_c3 <- .add_dimnames_from(expected_c3, actual_c3)
  expect_equal(actual_c3, expected_c3)

  actual_c2   <- dyadicMarkov:::countTheoBivariateC2(empirical)
  expected_c2 <- countTheoBivariateC2_old(empirical)
  expected_c2 <- .add_dimnames_from(expected_c2, actual_c2)
  expect_equal(actual_c2, expected_c2)
})


test_that("aicBivariate matches legacy", {
  skip_if_not(dir.exists("legacy") && file.exists(file.path("legacy", "FunctionsFile.R")),
              "legacy code not shipped in CRAN-minimal build")

  legacy <- .load_legacy()

  set.seed(888)
  states <- 2L

  emp <- matrix(sample(0:50, 4 * states^3, replace = TRUE),
                nrow = 4 * states * states, ncol = states)

  pop <- matrix(sample(c(0, 1:30), length(emp), replace = TRUE),
                nrow = nrow(emp), ncol = ncol(emp))

  expect_identical(
    dyadicMarkov:::aicBivariate(pop, emp, test = "single"),
    legacy$aicBivariate(pop, emp, test = "single")
  )
})

