test_that("univariatePattern treats p equal to alpha as rejection", {
  fm_am_equal <- c(2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L)
  sm_am_equal <- c(1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L)

  baseline_am <- dyadicMarkov::univariatePattern(
    fm_am_equal, sm_am_equal,
    states = 2L
  )
  alpha_am <- unname(baseline_am$TEST.AM$p.value)
  result_am <- dyadicMarkov::univariatePattern(
    fm_am_equal, sm_am_equal,
    states = 2L,
    alpha = alpha_am
  )

  expect_identical(unname(result_am$TEST.AM$p.value), alpha_am)
  expect_gt(unname(result_am$TEST.PM$p.value), alpha_am)
  expect_identical(result_am$pattern, "PM (A3)")

  fm_pm_equal <- c(1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L)
  sm_pm_equal <- c(2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L)

  baseline_pm <- dyadicMarkov::univariatePattern(
    fm_pm_equal, sm_pm_equal,
    states = 2L
  )
  alpha_pm <- unname(baseline_pm$TEST.PM$p.value)
  result_pm <- dyadicMarkov::univariatePattern(
    fm_pm_equal, sm_pm_equal,
    states = 2L,
    alpha = alpha_pm
  )

  expect_gt(unname(result_pm$TEST.AM$p.value), alpha_pm)
  expect_identical(unname(result_pm$TEST.PM$p.value), alpha_pm)
  expect_identical(result_pm$pattern, "AM (A2)")

  fm_both_equal <- c(1L, 2L, 1L, 1L)
  sm_both_equal <- c(2L, 2L, 1L, 1L)

  baseline_both <- dyadicMarkov::univariatePattern(
    fm_both_equal, sm_both_equal,
    states = 2L
  )
  alpha_both <- unname(baseline_both$TEST.AM$p.value)
  result_both <- dyadicMarkov::univariatePattern(
    fm_both_equal, sm_both_equal,
    states = 2L,
    alpha = alpha_both
  )

  expect_identical(unname(result_both$TEST.AM$p.value), alpha_both)
  expect_identical(unname(result_both$TEST.PM$p.value), alpha_both)
  expect_identical(result_both$pattern, "APM (A1)")
})
