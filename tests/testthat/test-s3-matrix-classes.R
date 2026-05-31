test_that("mleEstimation returns a dyadic_mle matrix with unchanged values", {
  empirical <- matrix(
    c(
      1, 1,
      0, 0,
      0, 2,
      3, 1
    ),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )

  fit <- dyadicMarkov::mleEstimation(empirical)
  expected <- matrix(
    c(
      0.5,  0.5,
      0.5,  0.5,
      0.0,  1.0,
      0.75, 0.25
    ),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )

  expect_s3_class(fit, "dyadic_mle")
  expect_true(is.matrix(fit))
  expect_equal(unclass(fit), expected)
  expect_true(all(abs(rowSums(fit) - 1) < 1e-10))
})


test_that("empirical count functions return dyadic_counts matrices", {
  chainFM <- c(1L, 2L, 1L)
  chainSM <- c(2L, 1L, 1L)

  counts <- dyadicMarkov::countEmp(chainFM, chainSM, states = 2L)
  expected_counts <- matrix(
    c(
      0L, 0L,
      0L, 1L,
      1L, 0L,
      0L, 0L
    ),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )

  expect_s3_class(counts, "dyadic_counts")
  expect_true(is.matrix(counts))
  expect_equal(unclass(counts), expected_counts)
  expect_equal(dim(counts), c(4L, 2L))

  chainFM_V1 <- c(1L, 2L, 1L)
  chainSM_V1 <- c(1L, 1L, 2L)
  chainFM_V2 <- c(2L, 1L, 2L)
  chainSM_V2 <- c(1L, 2L, 2L)

  bivar_counts <- dyadicMarkov::countEmpBivariate(
    chainFM_V1, chainSM_V1, chainFM_V2, chainSM_V2, states = 2L
  )
  expected_bivar_counts <- matrix(0L, nrow = 16L, ncol = 2L)
  expected_bivar_counts[3L, 2L] <- 1L
  expected_bivar_counts[10L, 1L] <- 1L

  expect_s3_class(bivar_counts, "dyadic_counts")
  expect_true(is.matrix(bivar_counts))
  expect_equal(unclass(bivar_counts), expected_bivar_counts)
  expect_equal(dim(bivar_counts), c(16L, 2L))
})
