test_that("bivariateCase treats p equal to alpha as rejection", {
  fm_v1_a1_equal <- c(1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L)
  sm_v1_a1_equal <- c(1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L)
  fm_v2_a1_equal <- c(1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L)
  sm_v2_a1_equal <- c(2L, 1L, 2L, 1L, 2L, 1L, 1L, 1L)

  empirical_a1 <- dyadicMarkov::countEmpBivariate(
    fm_v1_a1_equal, sm_v1_a1_equal,
    fm_v2_a1_equal, sm_v2_a1_equal,
    states = 2L
  )
  baseline_a1 <- dyadicMarkov::bivariateCase(empirical_a1)
  alpha_a1 <- unname(baseline_a1$testUnivariate$p.value)
  result_a1 <- dyadicMarkov::bivariateCase(empirical_a1, alpha = alpha_a1)

  expect_identical(unname(result_a1$testUnivariate$p.value), alpha_a1)
  expect_gt(unname(result_a1$testPartial$p.value), alpha_a1)
  expect_identical(result_a1$case, "partial")

  fm_v1_b1_equal <- c(1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L)
  sm_v1_b1_equal <- c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L)
  fm_v2_b1_equal <- c(1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L)
  sm_v2_b1_equal <- c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L)

  empirical_b1 <- dyadicMarkov::countEmpBivariate(
    fm_v1_b1_equal, sm_v1_b1_equal,
    fm_v2_b1_equal, sm_v2_b1_equal,
    states = 2L
  )
  baseline_b1 <- dyadicMarkov::bivariateCase(empirical_b1)
  alpha_b1 <- unname(baseline_b1$testPartial$p.value)
  result_b1 <- dyadicMarkov::bivariateCase(empirical_b1, alpha = alpha_b1)

  expect_gt(unname(result_b1$testUnivariate$p.value), alpha_b1)
  expect_identical(unname(result_b1$testPartial$p.value), alpha_b1)
  expect_identical(result_b1$case, "univariate")

  fm_v1_both_equal <- c(1L, 1L, 2L, 1L)
  sm_v1_both_equal <- c(1L, 1L, 1L, 1L)
  fm_v2_both_equal <- c(2L, 1L, 1L, 1L)
  sm_v2_both_equal <- c(1L, 1L, 1L, 1L)

  empirical_both <- dyadicMarkov::countEmpBivariate(
    fm_v1_both_equal, sm_v1_both_equal,
    fm_v2_both_equal, sm_v2_both_equal,
    states = 2L
  )
  baseline_both <- dyadicMarkov::bivariateCase(empirical_both)
  alpha_both <- unname(baseline_both$testUnivariate$p.value)
  result_both <- dyadicMarkov::bivariateCase(
    empirical_both,
    alpha = alpha_both
  )

  expect_identical(
    unname(result_both$testUnivariate$p.value),
    alpha_both
  )
  expect_identical(unname(result_both$testPartial$p.value), alpha_both)
  expect_identical(result_both$case, "complete")
})
