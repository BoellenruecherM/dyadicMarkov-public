pearson_x_squared <- function(observed, expected) {
  positive_expected <- expected > 0
  sum(
    (observed[positive_expected] - expected[positive_expected])^2 /
      expected[positive_expected]
  )
}


g_squared_deviance <- function(observed, expected) {
  positive <- observed > 0 & expected > 0
  2 * sum(observed[positive] * log(observed[positive] / expected[positive]))
}


candidate_aic <- function(observed, expected, k) {
  if (any(observed > 0 & expected == 0)) {
    return(Inf)
  }

  g_squared_deviance(observed, expected) + 2 * k
}


test_that("univariate LRT comparisons use Pearson's chi-squared statistic", {
  utils::data(
    "dyadic_univariate_example",
    package = "dyadicMarkov",
    envir = environment()
  )

  observed <- dyadicMarkov::countEmp(
    dyadic_univariate_example$FM,
    dyadic_univariate_example$SM,
    states = 2L
  )
  expected <- list(
    AM = dyadicMarkov:::countTheo(observed, "AM"),
    PM = dyadicMarkov:::countTheo(observed, "PM")
  )
  result <- dyadicMarkov::univariatePattern(
    dyadic_univariate_example$FM,
    dyadic_univariate_example$SM,
    states = 2L,
    alpha = 0.05
  )
  tests <- list(AM = result$TEST.AM, PM = result$TEST.PM)

  for (model in names(tests)) {
    pearson <- pearson_x_squared(observed, expected[[model]])
    g_squared <- g_squared_deviance(observed, expected[[model]])

    expect_equal(unname(tests[[model]]$statistic), pearson, tolerance = 1e-12)
    expect_gt(abs(pearson - g_squared), 1e-6)
    expect_equal(
      unname(tests[[model]]$p.value),
      stats::pchisq(pearson, df = 2L, lower.tail = FALSE),
      tolerance = 1e-12
    )
  }

  expect_identical(
    result$TEST.AM$method,
    "Likelihood-ratio test, Actor-only model"
  )
  expect_identical(
    result$TEST.PM$method,
    "Likelihood-ratio test, Partner-only model"
  )
})


test_that("global bivariate tests report Pearson's chi-squared statistic", {
  utils::data(
    "dyadic_bivariate_example",
    package = "dyadicMarkov",
    envir = environment()
  )

  observed <- dyadicMarkov::countEmpBivariate(
    dyadic_bivariate_example$FM_V1,
    dyadic_bivariate_example$SM_V1,
    dyadic_bivariate_example$FM_V2,
    dyadic_bivariate_example$SM_V2,
    states = 2L
  )
  expected <- dyadicMarkov:::countTheoBivariateG(observed)
  result <- dyadicMarkov::bivariateCase(observed, alpha = 0.05)
  tests <- list(result$testUnivariate, result$testPartial)

  for (index in seq_along(tests)) {
    pearson <- pearson_x_squared(observed, expected[[index]])
    g_squared <- g_squared_deviance(observed, expected[[index]])

    expect_equal(unname(tests[[index]]$statistic), pearson, tolerance = 1e-12)
    expect_gt(abs(pearson - g_squared), 1e-6)
    expect_identical(tests[[index]]$method, "Chi-squared test")
    expect_identical(unname(tests[[index]]$parameter), 12)
    expect_equal(
      unname(tests[[index]]$p.value),
      stats::pchisq(pearson, df = 12L, lower.tail = FALSE),
      tolerance = 1e-12
    )
  }
})


test_that("partial bivariate AIC uses G-squared rather than Pearson's statistic", {
  utils::data(
    "dyadic_bivariate_example",
    package = "dyadicMarkov",
    envir = environment()
  )

  observed <- dyadicMarkov::countEmpBivariate(
    dyadic_bivariate_example$FM_V1,
    dyadic_bivariate_example$SM_V1,
    dyadic_bivariate_example$FM_V2,
    dyadic_bivariate_example$SM_V2,
    states = 2L
  )
  global <- dyadicMarkov:::countTheoBivariateG(observed)
  local <- dyadicMarkov:::countTheoBivariateP(observed)
  expected <- c(list(global[[2L]]), local)
  k <- c(4L, 2L, 2L)

  manual <- vapply(
    seq_along(expected),
    function(index) candidate_aic(observed, expected[[index]], k[[index]]),
    numeric(1)
  )
  result <- dyadicMarkov::partialPattern(observed)

  expect_equal(result$aic$aic, manual, tolerance = 1e-12)

  pearson_based <- pearson_x_squared(observed, expected[[2L]]) + 2 * k[[2L]]
  expect_gt(abs(manual[[2L]] - pearson_based), 1e-6)
})


test_that("complete bivariate AIC uses G-squared before model penalties", {
  utils::data(
    "dyadic_bivariate_example",
    package = "dyadicMarkov",
    envir = environment()
  )

  observed <- dyadicMarkov::countEmpBivariate(
    dyadic_bivariate_example$FM_V1,
    dyadic_bivariate_example$SM_V1,
    dyadic_bivariate_example$FM_V2,
    dyadic_bivariate_example$SM_V2,
    states = 2L
  )
  expected <- c(
    list(C = observed),
    stats::setNames(
      dyadicMarkov:::countTheoBivariateC3(observed),
      c("D1", "D2", "D3", "D4")
    ),
    stats::setNames(
      dyadicMarkov:::countTheoBivariateC2(observed),
      c("E1", "E2", "E3", "E4")
    )
  )
  k <- c(
    C = 16L,
    D1 = 8L, D2 = 8L, D3 = 8L, D4 = 8L,
    E1 = 4L, E2 = 4L, E3 = 4L, E4 = 4L
  )
  manual <- vapply(
    names(expected),
    function(code) candidate_aic(observed, expected[[code]], k[[code]]),
    numeric(1)
  )

  result <- dyadicMarkov::completePattern(observed)

  expect_equal(result$aic$aic, unname(manual), tolerance = 1e-12)
})


test_that("univariate supports three states while bivariate remains binary", {
  chain_fm <- c(1L, 2L, 3L, 2L, 1L, 3L, 1L, 2L, 3L)
  chain_sm <- c(3L, 1L, 2L, 1L, 3L, 2L, 2L, 3L, 1L)

  counts <- dyadicMarkov::countEmp(chain_fm, chain_sm, states = 3L)
  estimates <- dyadicMarkov::mleEstimation(counts)
  pattern <- dyadicMarkov::univariatePattern(
    chain_fm,
    chain_sm,
    states = 3L,
    alpha = 0.05
  )

  expect_identical(dim(counts), c(9L, 3L))
  expect_identical(dim(estimates), c(9L, 3L))
  expect_identical(pattern$states, 3L)
  expect_identical(unname(pattern$TEST.AM$parameter), 12)

  expect_error(
    dyadicMarkov::countEmpBivariate(
      chain_fm, chain_sm, chain_fm, chain_sm,
      states = 3L
    ),
    "bivariate functions currently support states = 2 only.",
    fixed = TRUE
  )
})
