#' @srrstats {EA6.0c} Summary-table tests verify stable column names and source-faithful column and candidate ordering for univariate tests, bivariate global tests, and partial and complete AIC comparisons.
#' @srrstats {EA6.0d} Summary-table tests verify expected dimensions and column types for restriction/model labels, statistics, degrees of freedom, p-values, decisions, AIC values, Delta AIC values, minimum indicators, and selected indicators.
#' @srrstats {EA6.0e} Summary-table tests verify inferential and derived values, including p-value decisions, Delta AIC calculations, selected-pattern consistency, numerical AIC ties, and non-finite AIC handling.
#' @noRd
NULL
make_bivariate_summary_example <- function() {

  chainFM_V1 <- c(1L, 2L, 1L, 2L, 2L, 1L)
  chainSM_V1 <- c(2L, 1L, 2L, 1L, 1L, 2L)
  chainFM_V2 <- c(1L, 1L, 2L, 2L, 1L, 2L)
  chainSM_V2 <- c(2L, 2L, 1L, 1L, 2L, 1L)

  dyadicMarkov::countEmpBivariate(
    chainFM_V1,
    chainSM_V1,
    chainFM_V2,
    chainSM_V2,
    states = 2L
  )
}


expected_decision <- function(p_value, alpha) {

  valid <-
    is.finite(p_value) &
    p_value >= 0 &
    p_value <= 1

  out <- rep(NA_character_, length(p_value))

  out[valid] <- ifelse(
    p_value[valid] <= alpha,
    "Rejected",
    "Not rejected"
  )

  out
}


expected_aic_augmentation <- function(aic) {

  finite <- is.finite(aic)

  delta <- aic
  minimum <- rep(FALSE, length(aic))

  if (any(finite)) {

    min_aic <- min(aic[finite])

    delta[finite] <-
      aic[finite] - min_aic

    tolerance <-
      sqrt(.Machine$double.eps) *
      max(
        1,
        abs(min_aic)
      )

    minimum[finite] <-
      abs(
        aic[finite] -
          min_aic
      ) <= tolerance

  } else {

    delta[] <- NA_real_
  }

  list(
    delta = delta,
    minimum = minimum
  )
}


test_that("univariate summary table preserves inferential evidence", {

  chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
  chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)

  alpha <- 0.05

  object <- dyadicMarkov::univariatePattern(
    chainFM,
    chainSM,
    states = 2L,
    alpha = alpha
  )

  out <- summary(object)

  expect_s3_class(
    out,
    "summary_dyadic_pattern"
  )

  expect_named(
    out,
    c(
      "pattern",
      "tests",
      "alpha",
      "states",
      "call"
    )
  )

  expect_identical(
    names(out$tests),
    c(
      "restriction",
      "statistic",
      "df",
      "p_value",
      "decision"
    )
  )

  expect_identical(
    nrow(out$tests),
    2L
  )

  expect_identical(
    out$tests$restriction,
    c(
      "AM (A2): actor-only restriction",
      "PM (A3): partner-only restriction"
    )
  )

  expect_type(
    out$tests$restriction,
    "character"
  )

  expect_true(
    is.numeric(out$tests$statistic)
  )

  expect_true(
    is.numeric(out$tests$df)
  )

  expect_true(
    is.numeric(out$tests$p_value)
  )

  expect_type(
    out$tests$decision,
    "character"
  )

  expect_equal(
    out$tests$statistic,
    c(
      unname(object$TEST.AM$statistic),
      unname(object$TEST.PM$statistic)
    )
  )

  expect_equal(
    out$tests$df,
    c(
      unname(object$TEST.AM$parameter),
      unname(object$TEST.PM$parameter)
    )
  )

  expect_equal(
    out$tests$p_value,
    c(
      object$TEST.AM$p.value,
      object$TEST.PM$p.value
    )
  )

  expect_identical(
    out$tests$decision,
    expected_decision(
      out$tests$p_value,
      alpha
    )
  )

  expect_identical(
    out$pattern,
    object$pattern
  )

  expect_identical(
    out$alpha,
    object$alpha
  )

  expect_identical(
    out$states,
    object$states
  )
})


test_that("bivariate case summary table preserves global comparisons", {

  empirical <-
    make_bivariate_summary_example()

  alpha <- 0.05

  object <-
    dyadicMarkov::bivariateCase(
      empirical,
      alpha = alpha
    )

  out <- summary(object)

  expect_s3_class(
    out,
    "summary_dyadic_case"
  )

  expect_named(
    out,
    c(
      "case",
      "tests",
      "alpha",
      "call"
    )
  )

  expect_identical(
    names(out$tests),
    c(
      "model",
      "statistic",
      "df",
      "p_value",
      "decision"
    )
  )

  expect_identical(
    nrow(out$tests),
    2L
  )

  expect_identical(
    out$tests$model,
    c(
      "Main-variable-only model (A1)",
      "Second-variable-only model (B1)"
    )
  )

  expect_type(
    out$tests$model,
    "character"
  )

  expect_true(
    is.numeric(out$tests$statistic)
  )

  expect_true(
    is.numeric(out$tests$df)
  )

  expect_true(
    is.numeric(out$tests$p_value)
  )

  expect_type(
    out$tests$decision,
    "character"
  )

  expect_equal(
    out$tests$statistic,
    c(
      unname(object$testUnivariate$statistic),
      unname(object$testPartial$statistic)
    )
  )

  expect_equal(
    out$tests$df,
    c(
      unname(object$testUnivariate$parameter),
      unname(object$testPartial$parameter)
    )
  )

  expect_equal(
    out$tests$p_value,
    c(
      object$testUnivariate$p.value,
      object$testPartial$p.value
    )
  )

  expect_identical(
    out$tests$decision,
    expected_decision(
      out$tests$p_value,
      alpha
    )
  )

  expect_identical(
    out$case,
    object$case
  )

  expect_identical(
    out$alpha,
    object$alpha
  )
})


test_that("invalid p-values produce unavailable decisions", {

  chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
  chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)

  object <- dyadicMarkov::univariatePattern(
    chainFM,
    chainSM,
    states = 2L,
    alpha = 0.05
  )

  object$TEST.AM$p.value <- Inf
  object$TEST.PM$p.value <- NaN

  out <- summary(object)

  expect_true(
    all(is.na(out$tests$decision))
  )
})


test_that("partial AIC summary has stable schema and source order", {

  empirical <-
    make_bivariate_summary_example()

  object <-
    dyadicMarkov::partialPattern(
      empirical
    )

  out <- summary(object)

  expect_s3_class(
    out,
    "summary_dyadic_pattern"
  )

  expect_identical(
    names(out$aic),
    c(
      "pattern",
      "matrix",
      "aic",
      "delta_aic",
      "minimum",
      "selected"
    )
  )

  expect_identical(
    nrow(out$aic),
    3L
  )

  expect_identical(
    out$aic$matrix,
    c(
      "B1",
      "B2",
      "B3"
    )
  )

  expect_type(
    out$aic$pattern,
    "character"
  )

  expect_type(
    out$aic$matrix,
    "character"
  )

  expect_true(
    is.numeric(out$aic$aic)
  )

  expect_true(
    is.numeric(out$aic$delta_aic)
  )

  expect_type(
    out$aic$minimum,
    "logical"
  )

  expect_type(
    out$aic$selected,
    "logical"
  )

  expect_identical(
    out$aic$pattern,
    object$aic$pattern
  )

  expect_identical(
    out$aic$matrix,
    object$aic$matrix
  )

  expect_equal(
    out$aic$aic,
    object$aic$aic
  )

  expected <-
    expected_aic_augmentation(
      object$aic$aic
    )

  expect_equal(
    out$aic$delta_aic,
    expected$delta
  )

  expect_identical(
    out$aic$minimum,
    expected$minimum
  )

  expect_identical(
    out$aic$selected,
    object$aic$pattern == object$pattern
  )

  expect_identical(
    sum(out$aic$selected),
    1L
  )

  expect_identical(
    out$aic$pattern[out$aic$selected],
    object$pattern
  )

  expect_equal(
    out$aic$delta_aic[out$aic$selected],
    0
  )
})


test_that("complete AIC summary has stable schema and source order", {

  empirical <-
    make_bivariate_summary_example()

  object <-
    dyadicMarkov::completePattern(
      empirical
    )

  out <- summary(object)

  expect_identical(
    names(out$aic),
    c(
      "pattern",
      "matrix",
      "aic",
      "delta_aic",
      "minimum",
      "selected"
    )
  )

  expect_identical(
    nrow(out$aic),
    9L
  )

  expect_identical(
    out$aic$matrix,
    c(
      "C",
      "D1",
      "D2",
      "D3",
      "D4",
      "E1",
      "E2",
      "E3",
      "E4"
    )
  )

  expect_type(
    out$aic$pattern,
    "character"
  )

  expect_type(
    out$aic$matrix,
    "character"
  )

  expect_true(
    is.numeric(out$aic$aic)
  )

  expect_true(
    is.numeric(out$aic$delta_aic)
  )

  expect_type(
    out$aic$minimum,
    "logical"
  )

  expect_type(
    out$aic$selected,
    "logical"
  )

  expect_identical(
    out$aic$pattern,
    object$aic$pattern
  )

  expect_identical(
    out$aic$matrix,
    object$aic$matrix
  )

  expect_equal(
    out$aic$aic,
    object$aic$aic
  )

  expected <-
    expected_aic_augmentation(
      object$aic$aic
    )

  expect_equal(
    out$aic$delta_aic,
    expected$delta
  )

  expect_identical(
    out$aic$minimum,
    expected$minimum
  )

  expect_identical(
    out$aic$selected,
    object$aic$pattern == object$pattern
  )

  expect_identical(
    sum(out$aic$selected),
    1L
  )

  expect_identical(
    out$aic$pattern[out$aic$selected],
    object$pattern
  )

  expect_equal(
    out$aic$delta_aic[out$aic$selected],
    0
  )
})


test_that("AIC augmentation handles ties and non-finite values", {

  augment <-
    dyadicMarkov:::.augment_dyadic_aic_table

  exact_tie <- data.frame(
    pattern = c("P1", "P2", "P3"),
    matrix = c("M1", "M2", "M3"),
    aic = c(10, 10, 12),
    stringsAsFactors = FALSE
  )

  exact <-
    augment(
      exact_tie,
      "P1"
    )

  expect_equal(
    exact$delta_aic,
    c(0, 0, 2)
  )

  expect_identical(
    exact$minimum,
    c(TRUE, TRUE, FALSE)
  )

  expect_identical(
    exact$selected,
    c(TRUE, FALSE, FALSE)
  )


  near_tie <- exact_tie
  near_tie$aic <- c(
    10,
    10 + 1e-10,
    12
  )

  near <-
    augment(
      near_tie,
      "P1"
    )

  expect_identical(
    near$minimum,
    c(TRUE, TRUE, FALSE)
  )

  expect_equal(
    near$delta_aic,
    c(
      0,
      1e-10,
      2
    )
  )


  mixed <- exact_tie
  mixed$aic <- c(
    10,
    Inf,
    NA_real_
  )

  mixed_out <-
    augment(
      mixed,
      "P1"
    )

  expect_equal(
    mixed_out$delta_aic[1L],
    0
  )

  expect_true(
    is.infinite(
      mixed_out$delta_aic[2L]
    )
  )

  expect_true(
    is.na(
      mixed_out$delta_aic[3L]
    )
  )

  expect_identical(
    mixed_out$minimum,
    c(TRUE, FALSE, FALSE)
  )


  all_bad <- exact_tie
  all_bad$aic <- c(
    Inf,
    NA_real_,
    NaN
  )

  all_bad_out <-
    augment(
      all_bad,
      "P1"
    )

  expect_true(
    all(is.na(all_bad_out$delta_aic))
  )

  expect_identical(
    all_bad_out$minimum,
    c(FALSE, FALSE, FALSE)
  )

  expect_true(
    all(is.na(all_bad_out$selected))
  )
})


test_that("AIC augmentation validates its input table", {

  augment <-
    dyadicMarkov:::.augment_dyadic_aic_table

  expect_error(
    augment(
      data.frame(
        pattern = "P1",
        aic = 1
      ),
      "P1"
    ),
    "pattern, matrix, and aic"
  )

  expect_error(
    augment(
      data.frame(
        pattern = "P1",
        matrix = "M1",
        aic = "1"
      ),
      "P1"
    ),
    "must be numeric"
  )
})


test_that("summary print methods expose the intended table presentation", {

  empirical <-
    make_bivariate_summary_example()

  case <-
    dyadicMarkov::bivariateCase(
      empirical,
      alpha = 0.05
    )

  partial <-
    dyadicMarkov::partialPattern(
      empirical
    )

  case_output <-
    capture.output(
      print(
        summary(case)
      )
    )

  expect_true(
    any(
      grepl(
        "Global comparisons evaluated with Pearson Chi-squared",
        case_output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Main-variable-only model (A1)",
        case_output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Second-variable-only model (B1)",
        case_output,
        fixed = TRUE
      )
    )
  )


  aic_output <-
    capture.output(
      print(
        summary(partial)
      )
    )

  expect_true(
    any(
      grepl(
        "AIC candidate comparison",
        aic_output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Delta AIC",
        aic_output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Selected",
        aic_output,
        fixed = TRUE
      )
    )
  )

  expect_false(
    any(
      grepl(
        "Minimum",
        aic_output,
        fixed = TRUE
      )
    )
  )


  digits_2 <-
    capture.output(
      print(
        summary(partial),
        digits = 2L
      )
    )

  digits_5 <-
    capture.output(
      print(
        summary(partial),
        digits = 5L
      )
    )

  expect_false(
    identical(
      digits_2,
      digits_5
    )
  )
})


test_that("summary printing reports AIC ties and unavailable selection", {

  augment <-
    dyadicMarkov:::.augment_dyadic_aic_table

  tie_tab <- data.frame(
    pattern = c("P1", "P2", "P3"),
    matrix = c("M1", "M2", "M3"),
    aic = c(10, 10, 12),
    stringsAsFactors = FALSE
  )

  tie_summary <- list(
    pattern = "P1",
    aic = augment(
      tie_tab,
      "P1"
    ),
    call = NULL
  )

  class(tie_summary) <-
    c(
      "summary_dyadic_pattern",
      "list"
    )

  tie_output <-
    capture.output(
      print(tie_summary)
    )

  expect_true(
    any(
      grepl(
        "Multiple candidates share the minimum AIC",
        tie_output,
        fixed = TRUE
      )
    )
  )


  unavailable_tab <- tie_tab

  unavailable_tab$aic <- c(
    Inf,
    NA_real_,
    NaN
  )

  unavailable_summary <- list(
    pattern = "P1",
    aic = augment(
      unavailable_tab,
      "P1"
    ),
    call = NULL
  )

  class(unavailable_summary) <-
    c(
      "summary_dyadic_pattern",
      "list"
    )

  unavailable_output <-
    capture.output(
      print(unavailable_summary)
    )

  expect_true(
    any(
      grepl(
        "Selected pattern: Unavailable",
        unavailable_output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "No finite AIC candidate is available.",
        unavailable_output,
        fixed = TRUE
      )
    )
  )
})


test_that("summary print digits must be one non-negative integer", {

  empirical <-
    make_bivariate_summary_example()

  partial <-
    summary(
      dyadicMarkov::partialPattern(
        empirical
      )
    )

  case <-
    summary(
      dyadicMarkov::bivariateCase(
        empirical,
        alpha = 0.05
      )
    )

  for (bad in list(
    -1,
    1.5,
    NA_real_,
    c(2, 3)
  )) {

    expect_error(
      print(
        partial,
        digits = bad
      ),
      "one non-negative integer"
    )

    expect_error(
      print(
        case,
        digits = bad
      ),
      "one non-negative integer"
    )
  }
})
