#' @srrstats {G5.7} A deterministic regression test uses bundled sensitivity
#' simulation data to verify sequence-length scaling behaviour: for a known
#' fixed simulation fixtures at lengths 30 and 90 are checked independently;
#' the 30-point fixture is classified as trivial, whereas the 90-point fixture
#' recovers the expected A1, B2, D4, and E4 interaction patterns.
#' @noRd
NULL
.sensitivity_chain <- function(data, length, dyad, variable, member) {
  measurement_columns <- paste0("TM", seq_len(length))

  keep <-
    as.character(data$dyad) == as.character(dyad) &
    as.character(data$variable) == variable &
    as.character(data$members) == as.character(member)

  as.integer(
    unlist(
      data[
        keep,
        measurement_columns,
        drop = FALSE
      ],
      use.names = FALSE
    )
  )
}


.identify_sensitivity_pattern <- function(
    chainFM_V1,
    chainSM_V1,
    chainFM_V2,
    chainSM_V2) {

  empirical <- dyadicMarkov::countEmpBivariate(
    chainFM_V1,
    chainSM_V1,
    chainFM_V2,
    chainSM_V2,
    states = 2L
  )

  global <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  switch(
    global$case,
    trivial = "trivial",
    univariate = dyadicMarkov::univariatePattern(
      chainFM = chainFM_V1,
      chainSM = chainSM_V1,
      states = 2L,
      alpha = 0.05
    )$pattern,
    partial = dyadicMarkov::partialPattern(
      empirical
    )$pattern,
    complete = dyadicMarkov::completePattern(
      empirical
    )$pattern,
    NA_character_
  )
}


.classify_sensitivity_dyad <- function(data, length, dyad) {
  x1 <- .sensitivity_chain(
    data,
    length,
    dyad,
    "X",
    1L
  )

  x2 <- .sensitivity_chain(
    data,
    length,
    dyad,
    "X",
    2L
  )

  y1 <- .sensitivity_chain(
    data,
    length,
    dyad,
    "Y",
    1L
  )

  y2 <- .sensitivity_chain(
    data,
    length,
    dyad,
    "Y",
    2L
  )

  c(
    A1 = .identify_sensitivity_pattern(
      x1, x2, y1, y2
    ),
    B2 = .identify_sensitivity_pattern(
      x2, x1, y2, y1
    ),
    D4 = .identify_sensitivity_pattern(
      y1, y2, x1, x2
    ),
    E4 = .identify_sensitivity_pattern(
      y2, y1, x2, x1
    )
  )
}


test_that("pattern identification improves across a known sequence-length fixture", {
  utils::data(
    "data_complete_30",
    package = "dyadicMarkov"
  )

  utils::data(
    "data_complete_90",
    package = "dyadicMarkov"
  )

  result_30 <- .classify_sensitivity_dyad(
    data_complete_30,
    length = 30L,
    dyad = 1L
  )

  result_90 <- .classify_sensitivity_dyad(
    data_complete_90,
    length = 90L,
    dyad = 1L
  )

  expect_identical(
    result_30,
    c(
      A1 = "trivial",
      B2 = "trivial",
      D4 = "trivial",
      E4 = "trivial"
    )
  )

  expect_identical(
    result_90,
    c(
      A1 = "APM (A1)",
      B2 = "partial actor only (B2)",
      D4 = "actor-partner on the main, actor only on the second (D4)",
      E4 = "complete actor only (E4)"
    )
  )
})
