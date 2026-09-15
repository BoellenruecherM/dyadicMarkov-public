chisquaredDist <- function(population, empirical){
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
  return(chidist)
}

countEmp <- function(states, chainFM, chainSM){
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
  return(count)
}

mleEstimation <- function(empirical){
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
  return(estimate)
}

countTheo <- function(empirical, pattern = c("AM","PM")){
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
  #countTheo[is.nan(countTheo)] <- states/ncol(empirical) #WARNING

  return(countTheo)
}

lrtLocal <- function(population, empirical){
  method      <- "Chisquared test"
  dataName   <- "Observed vs Estimated"
  alternative <- "The unrestricted model fits the data better" # I changed this from "The unrestrictive model fits the data better" on the 22/02/26
  khi2 <- chisquaredDist(population = population, empirical = empirical)
  degree <- (ncol(population)*(ncol(population)-1)^2)
  pValue <- pchisq(q=khi2, df=degree, lower.tail = F)
  names(khi2) <- "X-squared"
  names(degree) <- "df"
  TEST        <- list(method = method, data.name = dataName,
                      parameter = degree, alternative = alternative, statistic = khi2, p.value = pValue)
  class(TEST) <- "htest"
  TEST
}

univariatePattern <- function(states, chainFM, chainSM, alpha){

  methodAM      <- "Likelihood ratio test, Actor-only model"
  methodPM      <- "Likelihood ratio test, Partner-only model"
  data <- "Dyadic sequences"
  alternativeAM <- "The full model fits the data better"
  alternativePM <- "The full model fits the data better"

  emp <- countEmp(states = states, chainFM = chainFM, chainSM = chainSM)
  estMat <- mleEstimation(empirical = emp)

  theoAM <- countTheo(empirical = emp, pattern = "AM")
  theoPM <- countTheo(empirical = emp, pattern = "PM")

  lrtAM <- lrtLocal(population = theoAM, empirical = emp)
  degreeAM <- lrtAM$degree
  khi2AM <- lrtAM$statistic
  pvalueAM <- lrtAM$p.value

  lrtPM <- lrtLocal(population = theoPM, empirical = emp)
  degreePM <- lrtPM$degree
  khi2PM <- lrtPM$statistic
  pvaluePM <- lrtPM$p.value


  if(isTRUE(pvalueAM > alpha) & isTRUE(pvaluePM > alpha)){
    type <- "IM (A0)"
  }
  if(isTRUE(pvalueAM < alpha) & isTRUE(pvaluePM > alpha)){
    type <- "PM (A3)"
  }
  if(isTRUE(pvalueAM > alpha) & isTRUE(pvaluePM < alpha)){
    type <- "AM (A2)"
  }
  if(isTRUE(pvalueAM < alpha) & isTRUE(pvaluePM < alpha)){
    type <- "APM (A1)"
  }

  TEST1        <- list(method = methodAM, data.name = data,
                       parameter = degreeAM, alternative = alternativeAM,
                       statistic = khi2AM, p.value = pvalueAM)
  class(TEST1) <- "htest"

  TEST2        <- list(method = methodPM, data.name = data,
                       parameter = degreePM, alternative = alternativePM,
                       statistic = khi2PM, p.value = pvaluePM)
  class(TEST2) <- "htest"

  return(list(TEST.AM = TEST1, TEST.PM = TEST2, pattern = type))
}

countEmpBivariate <- function(states, chainFM_V1, chainSM_V1, chainFM_V2, chainSM_V2){
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
  return(count)
}

countTheoBivariateG <- function(empirical){

  # Bloc --------------------------------------------------------------------

  # A1
  blocA1.1 <- empirical[1:4,]
  blocA1.2 <- empirical[5:8,]
  blocA1.3 <- empirical[9:12,]
  blocA1.4 <- empirical[13:16,]

  # B1
  blocB1.1 <- empirical[c(1,5,9,13),]
  blocB1.2 <- empirical[c(2,6,10,14),]
  blocB1.3 <- empirical[c(3,7,11,15),]
  blocB1.4 <- empirical[c(4,8,12,16),]

  # A1 ----------------------------------------------------------------------

  # fill the theoretical

  theoA1 <- rbind(
    matrix(c(rep((sum(blocA1.1[,1])/sum(blocA1.1)),4), rep((1-(sum(blocA1.1[,1])/sum(blocA1.1))),4)),
           byrow = FALSE, ncol = 2)*rowSums(blocA1.1),
    matrix(c(rep((sum(blocA1.2[,1])/sum(blocA1.2)),4), rep((1-(sum(blocA1.2[,1])/sum(blocA1.2))),4)),
           byrow = FALSE, ncol = 2)*rowSums(blocA1.2),
    matrix(c(rep((sum(blocA1.3[,1])/sum(blocA1.3)),4), rep((1-(sum(blocA1.3[,1])/sum(blocA1.3))),4)),
           byrow = FALSE, ncol = 2)*rowSums(blocA1.3),
    matrix(c(rep((sum(blocA1.4[,1])/sum(blocA1.4)),4), rep((1-(sum(blocA1.4[,1])/sum(blocA1.4))),4)),
           byrow = FALSE, ncol = 2)*rowSums(blocA1.4)
  )

  # B1 ----------------------------------------------------------------------

  # empty matrix

  theoB1 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoB1[c(1,5,9,13),] <- matrix(c(rep((sum(blocB1.1[,1])/sum(blocB1.1)),4), rep((1-(sum(blocB1.1[,1])/sum(blocB1.1))),4)),
                                 byrow = FALSE, ncol = 2)*rowSums(blocB1.1)
  theoB1[c(2,6,10,14),] <- matrix(c(rep((sum(blocB1.2[,1])/sum(blocB1.2)),4), rep((1-(sum(blocB1.2[,1])/sum(blocB1.2))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocB1.2)
  theoB1[c(3,7,11,15),] <- matrix(c(rep((sum(blocB1.3[,1])/sum(blocB1.3)),4), rep((1-(sum(blocB1.3[,1])/sum(blocB1.3))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocB1.3)
  theoB1[c(4,8,12,16),] <- matrix(c(rep((sum(blocB1.4[,1])/sum(blocB1.4)),4), rep((1-(sum(blocB1.4[,1])/sum(blocB1.4))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocB1.4)

  # Apply 1/s if nan --------------------------------------------------------

  if(any(is.nan(theoA1))){
    row <- which(!complete.cases(theoA1))
    theoA1[row, ] <- 0
  }

  if(any(is.nan(theoB1))){
    row <- which(!complete.cases(theoB1))
    theoB1[row, ] <- 0
  }

  # Return the matrices -----------------------------------------------------

  return(list(theoA1, theoB1))

}

countTheoBivariateP <- function(empirical){

  # Bloc --------------------------------------------------------------------

  # B2
  blocB2.1 <- empirical[c(1,2,5,6,9,10,13,14),]
  blocB2.2 <- empirical[c(3,4,7,8,11,12,15,16),]

  # B3
  blocB3.1 <- empirical[c(1,3,5,7,9,11,13,15),]
  blocB3.2 <- empirical[c(2,4,6,8,10,12,14,16),]

  # B2 ----------------------------------------------------------------------

  # empty matrix

  theoB2 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoB2[c(1,2,5,6,9,10,13,14),] <- matrix(c(rep((sum(blocB2.1[,1])/sum(blocB2.1)),8), rep((1-(sum(blocB2.1[,1])/sum(blocB2.1))),8)),
                                           byrow = FALSE, ncol = 2)*rowSums(blocB2.1)
  theoB2[c(3,4,7,8,11,12,15,16),] <- matrix(c(rep((sum(blocB2.2[,1])/sum(blocB2.2)),8), rep((1-(sum(blocB2.2[,1])/sum(blocB2.2))),8)),
                                            byrow = FALSE, ncol = 2)*rowSums(blocB2.2)

  # B3 ----------------------------------------------------------------------

  # empty matrix

  theoB3 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoB3[c(1,3,5,7,9,11,13,15),] <- matrix(c(rep((sum(blocB3.1[,1])/sum(blocB3.1)),8), rep((1-(sum(blocB3.1[,1])/sum(blocB3.1))),8)),
                                           byrow = FALSE, ncol = 2)*rowSums(blocB3.1)
  theoB3[c(2,4,6,8,10,12,14,16),] <- matrix(c(rep((sum(blocB3.2[,1])/sum(blocB3.2)),8), rep((1-(sum(blocB3.2[,1])/sum(blocB3.2))),8)),
                                            byrow = FALSE, ncol = 2)*rowSums(blocB3.2)

  # Apply 1/s if nan --------------------------------------------------------

  if(any(is.nan(theoB2))){
    row <- which(!complete.cases(theoB2))
    theoB2[row, ] <- 0
  }

  if(any(is.nan(theoB3))){
    row <- which(!complete.cases(theoB3))
    theoB3[row, ] <- 0
  }

  # Return the matrices -----------------------------------------------------

  return(list(theoB2, theoB3))

}

countTheoBivariateC4 <- function(empirical){

  # Same matrix ------------------------------------------------------------

  theoC <- empirical

  return(theoC)
}

countTheoBivariateC3 <- function(empirical){

  # Bloc --------------------------------------------------------------------

  # D1
  blocD1.1 <- empirical[c(1,9),]
  blocD1.2 <- empirical[c(2,10),]
  blocD1.3 <- empirical[c(3,11),]
  blocD1.4 <- empirical[c(4,12),]
  blocD1.5 <- empirical[c(5,13),]
  blocD1.6 <- empirical[c(6,14),]
  blocD1.7 <- empirical[c(7,15),]
  blocD1.8 <- empirical[c(8,16),]

  # D2
  blocD2.1 <- empirical[c(1,5),]
  blocD2.2 <- empirical[c(2,6),]
  blocD2.3 <- empirical[c(3,7),]
  blocD2.4 <- empirical[c(4,8),]
  blocD2.5 <- empirical[c(9,13),]
  blocD2.6 <- empirical[c(10,14),]
  blocD2.7 <- empirical[c(11,15),]
  blocD2.8 <- empirical[c(12,16),]

  # D3
  blocD3.1 <- empirical[c(1,3),]
  blocD3.2 <- empirical[c(2,4),]
  blocD3.3 <- empirical[c(5,7),]
  blocD3.4 <- empirical[c(6,8),]
  blocD3.5 <- empirical[c(9,11),]
  blocD3.6 <- empirical[c(10,12),]
  blocD3.7 <- empirical[c(13,15),]
  blocD3.8 <- empirical[c(14,16),]

  # D4
  blocD4.1 <- empirical[c(1,2),]
  blocD4.2 <- empirical[c(3,4),]
  blocD4.3 <- empirical[c(5,6),]
  blocD4.4 <- empirical[c(7,8),]
  blocD4.5 <- empirical[c(9,10),]
  blocD4.6 <- empirical[c(11,12),]
  blocD4.7 <- empirical[c(13,14),]
  blocD4.8 <- empirical[c(15,16),]


  # D1 ----------------------------------------------------------------------

  # empty matrix

  theoD1 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoD1[c(1,9),] <- matrix(c(rep((sum(blocD1.1[,1])/sum(blocD1.1)),2), rep((1-(sum(blocD1.1[,1])/sum(blocD1.1))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD1.1)
  theoD1[c(2,10),] <- matrix(c(rep((sum(blocD1.2[,1])/sum(blocD1.2)),2), rep((1-(sum(blocD1.2[,1])/sum(blocD1.2))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.2)
  theoD1[c(3,11),] <- matrix(c(rep((sum(blocD1.3[,1])/sum(blocD1.3)),2), rep((1-(sum(blocD1.3[,1])/sum(blocD1.3))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.3)
  theoD1[c(4,12),] <- matrix(c(rep((sum(blocD1.4[,1])/sum(blocD1.4)),2), rep((1-(sum(blocD1.4[,1])/sum(blocD1.4))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.4)
  theoD1[c(5,13),] <- matrix(c(rep((sum(blocD1.5[,1])/sum(blocD1.5)),2), rep((1-(sum(blocD1.5[,1])/sum(blocD1.5))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.5)
  theoD1[c(6,14),] <- matrix(c(rep((sum(blocD1.6[,1])/sum(blocD1.6)),2), rep((1-(sum(blocD1.6[,1])/sum(blocD1.6))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.6)
  theoD1[c(7,15),] <- matrix(c(rep((sum(blocD1.7[,1])/sum(blocD1.7)),2), rep((1-(sum(blocD1.7[,1])/sum(blocD1.7))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.7)
  theoD1[c(8,16),] <- matrix(c(rep((sum(blocD1.8[,1])/sum(blocD1.8)),2), rep((1-(sum(blocD1.8[,1])/sum(blocD1.8))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD1.8)

  # C2 ----------------------------------------------------------------------

  # empty matrix

  theoD2 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoD2[c(1,5),] <- matrix(c(rep((sum(blocD2.1[,1])/sum(blocD2.1)),2), rep((1-(sum(blocD2.1[,1])/sum(blocD2.1))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD2.1)
  theoD2[c(2,6),] <- matrix(c(rep((sum(blocD2.2[,1])/sum(blocD2.2)),2), rep((1-(sum(blocD2.2[,1])/sum(blocD2.2))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD2.2)
  theoD2[c(3,7),] <- matrix(c(rep((sum(blocD2.3[,1])/sum(blocD2.3)),2), rep((1-(sum(blocD2.3[,1])/sum(blocD2.3))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD2.3)
  theoD2[c(4,8),] <- matrix(c(rep((sum(blocD2.4[,1])/sum(blocD2.4)),2), rep((1-(sum(blocD2.4[,1])/sum(blocD2.4))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD2.4)
  theoD2[c(9,13),] <- matrix(c(rep((sum(blocD2.5[,1])/sum(blocD2.5)),2), rep((1-(sum(blocD2.5[,1])/sum(blocD2.5))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD2.5)
  theoD2[c(10,14),] <- matrix(c(rep((sum(blocD2.6[,1])/sum(blocD2.6)),2), rep((1-(sum(blocD2.6[,1])/sum(blocD2.6))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD2.6)
  theoD2[c(11,15),] <- matrix(c(rep((sum(blocD2.7[,1])/sum(blocD2.7)),2), rep((1-(sum(blocD2.7[,1])/sum(blocD2.7))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD2.7)
  theoD2[c(12,16),] <- matrix(c(rep((sum(blocD2.8[,1])/sum(blocD2.8)),2), rep((1-(sum(blocD2.8[,1])/sum(blocD2.8))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD2.8)

  # D3 ----------------------------------------------------------------------

  # empty matrix

  theoD3 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoD3[c(1,3),] <- matrix(c(rep((sum(blocD3.1[,1])/sum(blocD3.1)),2), rep((1-(sum(blocD3.1[,1])/sum(blocD3.1))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD3.1)
  theoD3[c(2,4),] <- matrix(c(rep((sum(blocD3.2[,1])/sum(blocD3.2)),2), rep((1-(sum(blocD3.2[,1])/sum(blocD3.2))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD3.2)
  theoD3[c(5,7),] <- matrix(c(rep((sum(blocD3.3[,1])/sum(blocD3.3)),2), rep((1-(sum(blocD3.3[,1])/sum(blocD3.3))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD3.3)
  theoD3[c(6,8),] <- matrix(c(rep((sum(blocD3.4[,1])/sum(blocD3.4)),2), rep((1-(sum(blocD3.4[,1])/sum(blocD3.4))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD3.4)
  theoD3[c(9,11),] <- matrix(c(rep((sum(blocD3.5[,1])/sum(blocD3.5)),2), rep((1-(sum(blocD3.5[,1])/sum(blocD3.5))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD3.5)
  theoD3[c(10,12),] <- matrix(c(rep((sum(blocD3.6[,1])/sum(blocD3.6)),2), rep((1-(sum(blocD3.6[,1])/sum(blocD3.6))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD3.6)
  theoD3[c(13,15),] <- matrix(c(rep((sum(blocD3.7[,1])/sum(blocD3.7)),2), rep((1-(sum(blocD3.7[,1])/sum(blocD3.7))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD3.7)
  theoD3[c(14,16),] <- matrix(c(rep((sum(blocD3.8[,1])/sum(blocD3.8)),2), rep((1-(sum(blocD3.8[,1])/sum(blocD3.8))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD3.8)

  # D4 ----------------------------------------------------------------------

  # empty matrix

  theoD4 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical by bloc

  theoD4[c(1,2),] <- matrix(c(rep((sum(blocD4.1[,1])/sum(blocD4.1)),2), rep((1-(sum(blocD4.1[,1])/sum(blocD4.1))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD4.1)
  theoD4[c(3,4),] <- matrix(c(rep((sum(blocD4.2[,1])/sum(blocD4.2)),2), rep((1-(sum(blocD4.2[,1])/sum(blocD4.2))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD4.2)
  theoD4[c(5,6),] <- matrix(c(rep((sum(blocD4.3[,1])/sum(blocD4.3)),2), rep((1-(sum(blocD4.3[,1])/sum(blocD4.3))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD4.3)
  theoD4[c(7,8),] <- matrix(c(rep((sum(blocD4.4[,1])/sum(blocD4.4)),2), rep((1-(sum(blocD4.4[,1])/sum(blocD4.4))),2)),
                            byrow = FALSE, ncol = 2)*rowSums(blocD4.4)
  theoD4[c(9,10),] <- matrix(c(rep((sum(blocD4.5[,1])/sum(blocD4.5)),2), rep((1-(sum(blocD4.5[,1])/sum(blocD4.5))),2)),
                             byrow = FALSE, ncol = 2)*rowSums(blocD4.5)
  theoD4[c(11,12),] <- matrix(c(rep((sum(blocD4.6[,1])/sum(blocD4.6)),2), rep((1-(sum(blocD4.6[,1])/sum(blocD4.6))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD4.6)
  theoD4[c(13,14),] <- matrix(c(rep((sum(blocD4.7[,1])/sum(blocD4.7)),2), rep((1-(sum(blocD4.7[,1])/sum(blocD4.7))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD4.7)
  theoD4[c(15,16),] <- matrix(c(rep((sum(blocD4.8[,1])/sum(blocD4.8)),2), rep((1-(sum(blocD4.8[,1])/sum(blocD4.8))),2)),
                              byrow = FALSE, ncol = 2)*rowSums(blocD4.8)

  # Apply 1/s if nan --------------------------------------------------------

  if(any(is.nan(theoD1))){
    row <- which(!complete.cases(theoD1))
    theoD1[row, ] <- 0
  }

  if(any(is.nan(theoD2))){
    row <- which(!complete.cases(theoD2))
    theoD2[row, ] <- 0
  }

  if(any(is.nan(theoD3))){
    row <- which(!complete.cases(theoD3))
    theoD3[row, ] <- 0
  }

  if(any(is.nan(theoD4))){
    row <- which(!complete.cases(theoD4))
    theoD4[row, ] <- 0
  }

  # Return the matrices -----------------------------------------------------

  return(list(theoD1, theoD2, theoD3, theoD4))

}

countTheoBivariateC2 <- function(empirical){

  # Bloc --------------------------------------------------------------------

  # E1
  blocE1.1 <- empirical[c(1,3,9,11),]
  blocE1.2 <- empirical[c(2,4,10,12),]
  blocE1.3 <- empirical[c(5,7,13,15),]
  blocE1.4 <- empirical[c(6,8,14,16),]

  # E2
  blocE2.1 <- empirical[c(1:2,9:10),]
  blocE2.2 <- empirical[c(3:4,11:12),]
  blocE2.3 <- empirical[c(5:6,13:14),]
  blocE2.4 <- empirical[c(7:8,15:16),]

  # E3
  blocE3.1 <- empirical[c(1,3,5,7),]
  blocE3.2 <- empirical[c(2,4,6,8),]
  blocE3.3 <- empirical[c(9,11,13,15),]
  blocE3.4 <- empirical[c(10,12,14,16),]

  # E4
  blocE4.1 <- empirical[c(1:2,5:6),]
  blocE4.2 <- empirical[c(3:4,7:8),]
  blocE4.3 <- empirical[c(9:10,13:14),]
  blocE4.4 <- empirical[c(11:12,15:16),]

  # E1 ----------------------------------------------------------------------

  # empty matrix

  theoE1 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoE1[c(1,3,9,11),] <- matrix(c(rep((sum(blocE1.1[,1])/sum(blocE1.1)),4), rep((1-(sum(blocE1.1[,1])/sum(blocE1.1))),4)),
                                 byrow = FALSE, ncol = 2)*rowSums(blocE1.1)
  theoE1[c(2,4,10,12),] <- matrix(c(rep((sum(blocE1.2[,1])/sum(blocE1.2)),4), rep((1-(sum(blocE1.2[,1])/sum(blocE1.2))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE1.2)
  theoE1[c(5,7,13,15),] <- matrix(c(rep((sum(blocE1.3[,1])/sum(blocE1.3)),4), rep((1-(sum(blocE1.3[,1])/sum(blocE1.3))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE1.3)
  theoE1[c(6,8,14,16),] <- matrix(c(rep((sum(blocE1.4[,1])/sum(blocE1.4)),4), rep((1-(sum(blocE1.4[,1])/sum(blocE1.4))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE1.4)

  # E2 ----------------------------------------------------------------------

  # empty matrix

  theoE2 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoE2[c(1:2,9:10),] <- matrix(c(rep((sum(blocE2.1[,1])/sum(blocE2.1)),4), rep((1-(sum(blocE2.1[,1])/sum(blocE2.1))),4)),
                                 byrow = FALSE, ncol = 2)*rowSums(blocE2.1)
  theoE2[c(3:4,11:12),] <- matrix(c(rep((sum(blocE2.2[,1])/sum(blocE2.2)),4), rep((1-(sum(blocE2.2[,1])/sum(blocE2.2))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE2.2)
  theoE2[c(5:6,13:14),] <- matrix(c(rep((sum(blocE2.3[,1])/sum(blocE2.3)),4), rep((1-(sum(blocE2.3[,1])/sum(blocE2.3))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE2.3)
  theoE2[c(7:8,15:16),] <- matrix(c(rep((sum(blocE2.4[,1])/sum(blocE2.4)),4), rep((1-(sum(blocE2.4[,1])/sum(blocE2.4))),4)),
                                  byrow = FALSE, ncol = 2)*rowSums(blocE2.4)

  # E3 ----------------------------------------------------------------------

  # empty matrix

  theoE3 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical

  theoE3[c(1,3,5,7),] <- matrix(c(rep((sum(blocE3.1[,1])/sum(blocE3.1)),4), rep((1-(sum(blocE3.1[,1])/sum(blocE3.1))),4)),
                                byrow = FALSE, ncol = 2)*rowSums(blocE3.1)
  theoE3[c(2,4,6,8),] <- matrix(c(rep((sum(blocE3.2[,1])/sum(blocE3.2)),4), rep((1-(sum(blocE3.2[,1])/sum(blocE3.2))),4)),
                                byrow = FALSE, ncol = 2)*rowSums(blocE3.2)
  theoE3[c(9,11,13,15),] <- matrix(c(rep((sum(blocE3.3[,1])/sum(blocE3.3)),4), rep((1-(sum(blocE3.3[,1])/sum(blocE3.3))),4)),
                                   byrow = FALSE, ncol = 2)*rowSums(blocE3.3)
  theoE3[c(10,12,14,16),] <- matrix(c(rep((sum(blocE3.4[,1])/sum(blocE3.4)),4), rep((1-(sum(blocE3.4[,1])/sum(blocE3.4))),4)),
                                    byrow = FALSE, ncol = 2)*rowSums(blocE3.4)

  # E4 ----------------------------------------------------------------------

  # empty matrix

  theoE4 <- matrix(NA, ncol = ncol(empirical), nrow = nrow(empirical))

  # fill the theoretical by bloc

  theoE4[c(1:2,5:6),] <- matrix(c(rep((sum(blocE4.1[,1])/sum(blocE4.1)),4), rep((1-(sum(blocE4.1[,1])/sum(blocE4.1))),4)),
                                byrow = FALSE, ncol = 2)*rowSums(blocE4.1)
  theoE4[c(3:4,7:8),] <- matrix(c(rep((sum(blocE4.2[,1])/sum(blocE4.2)),4), rep((1-(sum(blocE4.2[,1])/sum(blocE4.2))),4)),
                                byrow = FALSE, ncol = 2)*rowSums(blocE4.2)
  theoE4[c(9:10,13:14),] <- matrix(c(rep((sum(blocE4.3[,1])/sum(blocE4.3)),4), rep((1-(sum(blocE4.3[,1])/sum(blocE4.3))),4)),
                                   byrow = FALSE, ncol = 2)*rowSums(blocE4.3)
  theoE4[c(11:12,15:16),] <- matrix(c(rep((sum(blocE4.4[,1])/sum(blocE4.4)),4), rep((1-(sum(blocE4.4[,1])/sum(blocE4.4))),4)),
                                    byrow = FALSE, ncol = 2)*rowSums(blocE4.4)

  # Apply 1/s if nan --------------------------------------------------------

  if(any(is.nan(theoE1))){
    row <- which(!complete.cases(theoE1))
    theoE1[row, ] <- 0
  }

  if(any(is.nan(theoE2))){
    row <- which(!complete.cases(theoE2))
    theoE2[row, ] <- 0
  }

  if(any(is.nan(theoE3))){
    row <- which(!complete.cases(theoE3))
    theoE3[row, ] <- 0
  }

  if(any(is.nan(theoE4))){
    row <- which(!complete.cases(theoE4))
    theoE4[row, ] <- 0
  }

  # Return the matrices -----------------------------------------------------

  return(list(theoE1, theoE2, theoE3, theoE4))

}

bivariateTest <- function(population, empirical){

  df <- 12

  # Parameters, distance and pvalue -----------------------------------------

  method      <- "Chisquared test"
  dataName   <- "Observed vs Estimated"
  alternative <- "The unrestricted model fits the data better" # I changed this from "The model with constraints fit the data better" on the 22/02/26 because it was wrong
  khi2 <- chisquaredDist(population = population, empirical = empirical)
  degree <- df
  pValue <- pchisq(q=khi2, df=degree, lower.tail = F)
  names(khi2) <- "X-squared"
  names(degree) <- "df"

  TEST        <- list(method = method, data.name = dataName,
                      parameter = degree, alternative = alternative, statistic = khi2, p.value = pValue)
  class(TEST) <- "htest"
  TEST

}

bivariateCase <- function(empirical, alpha){

  #global test

  theoA1 <- countTheoBivariateG(empirical)[[1]]
  theoB1 <- countTheoBivariateG(empirical)[[2]]

  univariate <- bivariateTest(population = theoA1, empirical = empirical)
  partial <- bivariateTest(population = theoB1, empirical = empirical)

  p.valueA1 <- univariate$p.value
  p.valueB1 <- partial$p.value

  if(isTRUE(p.valueA1>alpha) & isTRUE(p.valueB1>alpha)){
    case <- "trivial"
  }
  if(isTRUE(p.valueA1>alpha) & isTRUE(p.valueB1<alpha)){
    case <- "univariate"
  }
  if(isTRUE(p.valueA1<alpha) & isTRUE(p.valueB1>alpha)){
    case <- "partial"
  }
  if(isTRUE(p.valueA1<alpha) & isTRUE(p.valueB1<alpha)){
    case <- "complete"
  }

  return(list(testUnivariate = univariate, testPartial = partial, case = case))

}

aicBivariate <- function(population, empirical, test = c("single","duo", "triplet", "quadruplet")){

  #set the value of k, the # of parameters to estimate
  if(isTRUE(test=="single")){
    k <- 2
  }
  if(isTRUE(test=="duo")){
    k <- 4
  }
  if(isTRUE(test=="triplet")){
    k <- 8
  }
  if(isTRUE(test=="quadruplet")){
    k <- 16
  }

  #deviance
  G2 <- 2*sum(empirical * log(empirical/population), na.rm = TRUE)

  #AIC
  aic <- 2*k + G2

  return(aic)

}

partialPattern <- function(empirical){

  countTheoB1 <- countTheoBivariateG(empirical)[[2]]
  countTheoB2 <- countTheoBivariateP(empirical)[[1]]
  countTheoB3 <- countTheoBivariateP(empirical)[[2]]

  b1 <- aicBivariate(empirical = empirical, population = countTheoB1, test = "duo")
  b2 <- aicBivariate(empirical = empirical, population = countTheoB2, test = "single")
  b3 <- aicBivariate(empirical = empirical, population = countTheoB3, test = "single")

  aicMat <- matrix(
    c("partial actor partner (B1)", "B1", b1, "partial actor (B2)", "B2", b2, "partial partner (B3)", "B3", b3), ncol = 3,
    byrow = TRUE
  )

  aicMat <- as.data.frame(aicMat)
  colnames(aicMat) <- c("pattern", "matrix","aic")
  aicMat$aic <- as.numeric(aicMat$aic)

  aicVec <- list("partial actor partner (B1)" = b1, "partial actor (B2)" = b2, "partial partner (B3)" = b3)
  type <- names(aicVec)[which.min(aicVec)]

  return(list(aic = aicMat, pattern = type))

}

completePattern <- function(empirical){

  countTheoD1 <- countTheoBivariateC3(empirical)[[1]]
  countTheoD2 <- countTheoBivariateC3(empirical)[[2]]
  countTheoD3 <- countTheoBivariateC3(empirical)[[3]]
  countTheoD4 <- countTheoBivariateC3(empirical)[[4]]

  countTheoE1 <- countTheoBivariateC2(empirical)[[1]]
  countTheoE2 <- countTheoBivariateC2(empirical)[[2]]
  countTheoE3 <- countTheoBivariateC2(empirical)[[3]]
  countTheoE4 <- countTheoBivariateC2(empirical)[[4]]

  c <- aicBivariate(empirical = empirical, population = empirical, test = "quadruplet")

  d1 <- aicBivariate(empirical = empirical, population = countTheoD1, test = "triplet")
  d2 <- aicBivariate(empirical = empirical, population = countTheoD2, test = "triplet")
  d3 <- aicBivariate(empirical = empirical, population = countTheoD3, test = "triplet")
  d4 <- aicBivariate(empirical = empirical, population = countTheoD4, test = "triplet")

  e1 <- aicBivariate(empirical = empirical, population = countTheoE1, test = "duo")
  e2 <- aicBivariate(empirical = empirical, population = countTheoE2, test = "duo")
  e3 <- aicBivariate(empirical = empirical, population = countTheoE3, test = "duo")
  e4 <- aicBivariate(empirical = empirical, population = countTheoE4, test = "duo")

  aicMat <- matrix(
    c("complete actor partner on both (C)", "C", c,
      "complete partner on the main, actor partner on the second (D1)", "D1", d1,
      "complete actor on the main, actor partner on the second (D2)", "D2", d2,
      "complete actor partner on the main, partner on the second (D3)", "D3", d3,
      "complete actor partner on the main, actor on the second (D4)", "D4", d4,
      "complete partner on both (E1)", "E1", e1,
      "complete partner on the main, actor on the second (E2)", "E2", e2,
      "complete actor on the main, partner on the second (E3)", "E3", e3,
      "complete actor on both (E4)", "E4", e4), ncol = 3,
    byrow = TRUE
  )

  aicMat <- as.data.frame(aicMat)
  colnames(aicMat) <- c("pattern", "matrix","aic")
  aicMat$aic <- as.numeric(aicMat$aic)

  aicVec <- list(
    "complete actor partner on both (C)" = as.numeric(c),
    "complete partner on the main, actor partner on the second (D1)" = as.numeric(d1),
    "complete actor on the main, actor partner on the second (D2)" = as.numeric(d2),
    "complete actor partner on the main, partner on the second (D3)" = as.numeric(d3),
    "complete actor partner on the main, actor on the second (D4)" = as.numeric(d4),
    "complete partner both (E1)" = as.numeric(e1),
    "complete partner on the main, actor on the second (E2)" = as.numeric(e2),
    "complete actor on the main, partner on the second (E3)" = as.numeric(e3),
    "complete actor on both (E4)" = as.numeric(e4))

  type <- names(aicVec)[which.min(aicVec)]

  return(list(aic = aicMat, pattern = type))

}
