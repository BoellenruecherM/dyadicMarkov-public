# Extended reproduction test for the binary bivariate sequence-length
# sensitivity analysis.
#
# This file belongs to the ordinary testthat framework, but the expensive
# reproduction is run only when:
#
#   DYADICMARKOV_EXTENDED_TESTS=true
#
# The test uses the fixed simulation data bundled with dyadicMarkov. It does
# not regenerate the original stochastic simulations.

#' @srrstats {G5.4c} The opt-in extended sensitivity test compares the
#' current five-length reproduction directly with the sensitivity and
#' specificity values reported in Table 5 of Bollenrücher et al. (in press).
#' The comparison uses an explicit absolute tolerance to accommodate the small
#' specificity differences documented in the sensitivity-analysis vignette.
#' @noRd
NULL

.extended_sensitivity_chain <- function(
    data,
    n,
    dyad,
    variable,
    member) {

  measurement_columns <- paste0(
    "TM",
    seq_len(n)
  )

  keep <-
    as.character(data$dyad) == as.character(dyad) &
    as.character(data$variable) == variable &
    as.character(data$members) == as.character(member)

  if (sum(keep) != 1L) {
    stop(
      "Expected exactly one sensitivity sequence.",
      call. = FALSE
    )
  }

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


.extended_univariate_code <- function(x) {
  code <- sub(
    "^.*\\(([A-Z][0-9]+)\\)$",
    "\\1",
    x$pattern
  )

  if (identical(code, x$pattern)) {
    stop(
      "The univariate pattern code could not be extracted.",
      call. = FALSE
    )
  }

  code
}


.extended_selected_matrix_code <- function(x) {
  i <- match(
    x$pattern,
    x$aic$pattern
  )

  if (is.na(i)) {
    stop(
      "The selected pattern could not be matched to its AIC table.",
      call. = FALSE
    )
  }

  x$aic$matrix[i]
}


.extended_classify_local_pattern <- function(
    data,
    n,
    dyad,
    variable,
    member,
    alpha = 0.05) {

  other_member <- if (member == 1L) {
    2L
  } else {
    1L
  }

  second_variable <- if (variable == "X") {
    "Y"
  } else {
    "X"
  }

  main_first <- .extended_sensitivity_chain(
    data,
    n,
    dyad,
    variable,
    member
  )

  main_second <- .extended_sensitivity_chain(
    data,
    n,
    dyad,
    variable,
    other_member
  )

  second_first <- .extended_sensitivity_chain(
    data,
    n,
    dyad,
    second_variable,
    member
  )

  second_second <- .extended_sensitivity_chain(
    data,
    n,
    dyad,
    second_variable,
    other_member
  )

  empirical <- dyadicMarkov::countEmpBivariate(
    chainFM_V1 = main_first,
    chainSM_V1 = main_second,
    chainFM_V2 = second_first,
    chainSM_V2 = second_second,
    states = 2L
  )

  global <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = alpha
  )

  if (identical(global$case, "trivial")) {
    return("trivial")
  }

  if (identical(global$case, "univariate")) {
    local <- dyadicMarkov::univariatePattern(
      chainFM = main_first,
      chainSM = main_second,
      states = 2L,
      alpha = alpha
    )

    return(
      .extended_univariate_code(local)
    )
  }

  if (identical(global$case, "partial")) {
    local <- dyadicMarkov::partialPattern(
      empirical
    )

    return(
      .extended_selected_matrix_code(local)
    )
  }

  if (identical(global$case, "complete")) {
    local <- dyadicMarkov::completePattern(
      empirical
    )

    return(
      .extended_selected_matrix_code(local)
    )
  }

  stop(
    "Unknown or unavailable bivariate case.",
    call. = FALSE
  )
}


.extended_classify_one_length <- function(
    data,
    n,
    alpha = 0.05) {

  dyads <- sort(
    unique(
      as.integer(data$dyad)
    )
  )

  orientations <- data.frame(
    variable = c(
      "X",
      "X",
      "Y",
      "Y"
    ),
    member = c(
      1L,
      2L,
      1L,
      2L
    ),
    stringsAsFactors = FALSE
  )

  out <- vector(
    "list",
    length(dyads) * nrow(orientations)
  )

  k <- 1L

  for (d in dyads) {
    for (j in seq_len(nrow(orientations))) {

      variable <- orientations$variable[j]
      member <- orientations$member[j]

      keep <-
        as.character(data$dyad) == as.character(d) &
        as.character(data$variable) == variable &
        as.character(data$members) == as.character(member)

      simulated <- unique(
        as.character(
          data$caseSimulated[keep]
        )
      )

      if (length(simulated) != 1L) {
        stop(
          "Expected exactly one simulation label for dyad ",
          d,
          ", variable ",
          variable,
          ", member ",
          member,
          ".",
          call. = FALSE
        )
      }

      identified <- .extended_classify_local_pattern(
        data = data,
        n = n,
        dyad = d,
        variable = variable,
        member = member,
        alpha = alpha
      )

      out[[k]] <- data.frame(
        length = n,
        dyad = d,
        variable = variable,
        member = member,
        simulated = simulated,
        identified = identified,
        stringsAsFactors = FALSE
      )

      k <- k + 1L
    }
  }

  do.call(
    rbind,
    out
  )
}


.extended_sensitivity_table <- function() {
  utils::data(
    "data_complete_30",
    package = "dyadicMarkov"
  )

  utils::data(
    "data_complete_60",
    package = "dyadicMarkov"
  )

  utils::data(
    "data_complete_90",
    package = "dyadicMarkov"
  )

  utils::data(
    "data_complete_180",
    package = "dyadicMarkov"
  )

  utils::data(
    "data_complete_720",
    package = "dyadicMarkov"
  )

  simulation_data <- list(
    `30` = data_complete_30,
    `60` = data_complete_60,
    `90` = data_complete_90,
    `180` = data_complete_180,
    `720` = data_complete_720
  )

  classification_results <- do.call(
    rbind,
    lapply(
      names(simulation_data),
      function(n) {
        .extended_classify_one_length(
          data = simulation_data[[n]],
          n = as.integer(n),
          alpha = 0.05
        )
      }
    )
  )

  patterns <- c(
    "A1",
    "B2",
    "D4",
    "E4"
  )

  lengths <- c(
    30L,
    60L,
    90L,
    180L,
    720L
  )

  reproduced <- data.frame(
    Length = lengths
  )

  for (pattern in patterns) {

    sensitivity <- numeric(
      length(lengths)
    )

    specificity <- numeric(
      length(lengths)
    )

    for (i in seq_along(lengths)) {

      current <- classification_results[
        classification_results$length == lengths[i],
        ,
        drop = FALSE
      ]

      truth <- current$simulated == pattern
      identified <- current$identified == pattern

      TP <- sum(
        truth & identified
      )

      FN <- sum(
        truth & !identified
      )

      TN <- sum(
        !truth & !identified
      )

      FP <- sum(
        !truth & identified
      )

      sensitivity[i] <-
        TP / (TP + FN)

      specificity[i] <-
        TN / (TN + FP)
    }

    reproduced[[paste(pattern, "Se")]] <- round(
      sensitivity,
      3
    )

    reproduced[[paste(pattern, "Sp")]] <- round(
      specificity,
      3
    )
  }

  reproduced
}


test_that("full five-length sensitivity profile is reproduced", {
  testthat::skip_if(
    tolower(
      Sys.getenv(
        "DYADICMARKOV_EXTENDED_TESTS"
      )
    ) != "true",
    paste0(
      "Set DYADICMARKOV_EXTENDED_TESTS=true ",
      "to run the full sensitivity reproduction."
    )
  )

  observed <- .extended_sensitivity_table()

  # Expected values are the results obtained from the full sensitivity-analysis
  # reproduction using the bundled simulation data. They are the values displayed
  # in the sensitivity-analysis vignette and correspond to the sensitivity and
  # specificity results reported in Table 5 of Bollenrücher et al. (in press),
  # with minor numerical differences of at most 0.006 in a few specificity values.
  expected <- data.frame(
    Length = c(
      30L,
      60L,
      90L,
      180L,
      720L
    )
  )

  expected[["A1 Se"]] <- c(
    0.125,
    0.836,
    0.936,
    0.941,
    0.946
  )

  expected[["A1 Sp"]] <- c(
    0.992,
    0.942,
    0.951,
    0.997,
    1.000
  )

  expected[["B2 Se"]] <- c(
    0.114,
    0.680,
    0.801,
    0.835,
    0.850
  )

  expected[["B2 Sp"]] <- c(
    0.994,
    0.940,
    0.935,
    0.991,
    1.000
  )

  expected[["D4 Se"]] <- c(
    0.000,
    0.119,
    0.426,
    0.876,
    0.951
  )

  expected[["D4 Sp"]] <- c(
    0.999,
    0.972,
    0.955,
    0.952,
    0.960
  )

  expected[["E4 Se"]] <- c(
    0.015,
    0.358,
    0.621,
    0.765,
    0.808
  )

  expected[["E4 Sp"]] <- c(
    1.000,
    0.997,
    0.997,
    0.999,
    0.998
  )

  expect_equal(
    observed,
    expected,
    tolerance = 5e-4
  )

  # Published sensitivity and specificity values from Table 5 of
  # Bollenrücher et al. (in press).
  published <- data.frame(
    Length = c(
      30L,
      60L,
      90L,
      180L,
      720L
    ),
    check.names = FALSE
  )

  published[["A1 Se"]] <- c(
    0.125,
    0.836,
    0.936,
    0.941,
    0.946
  )

  published[["A1 Sp"]] <- c(
    0.992,
    0.942,
    0.951,
    0.997,
    1.000
  )

  published[["B2 Se"]] <- c(
    0.114,
    0.680,
    0.801,
    0.835,
    0.850
  )

  published[["B2 Sp"]] <- c(
    0.994,
    0.939,
    0.935,
    0.991,
    1.000
  )

  published[["D4 Se"]] <- c(
    0.000,
    0.119,
    0.426,
    0.876,
    0.951
  )

  published[["D4 Sp"]] <- c(
    0.993,
    0.972,
    0.955,
    0.952,
    0.960
  )

  published[["E4 Se"]] <- c(
    0.015,
    0.358,
    0.621,
    0.765,
    0.808
  )

  published[["E4 Sp"]] <- c(
    0.999,
    0.997,
    0.997,
    0.998,
    0.998
  )

  expect_identical(
    observed$Length,
    published$Length
  )

  maximum_published_difference <- max(
    abs(
      as.matrix(observed[, -1, drop = FALSE]) -
        as.matrix(published[, -1, drop = FALSE])
    )
  )

  expect_lte(
    maximum_published_difference,
    0.01
  )
})
