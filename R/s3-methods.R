#' Print a line with newline termination
#'
#' Internal helper used by print methods.
#'
#' @param ... Objects passed to `cat()`.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
#' @srrstats {EA4.0} Empirical transition counts are returned as integer-valued matrix-like objects, MLE estimates as numeric probability matrices, inferential statistics and AIC values as numeric values, and identified patterns, cases, and decisions as character outputs.
#' @srrstats {EA4.1} Matrix-like print methods and inferential summary print methods provide a digits argument to explicitly control numeric precision in screen output.
#' @srrstats {EA4.2} Primary pattern and case objects provide concise print methods and richer summary tables, while empirical counts and MLE probabilities retain their natural matrix-like representation.
#' @srrstats {EA5.2} Matrix-like outputs use formatC() for controlled numeric display, while inferential summary tables use formatC() and format.pval() so statistics, AIC values, and p-values are displayed with controlled precision.
#' @srrstats {EA5.3} Summary methods for matrix-like dyadic objects report object class, storage mode, dimensions, and row and column names.
.cat_line <- function(...) {
  cat(..., "\n", sep = "")
  invisible(NULL)
}


#' Validate display precision
#'
#' Internal helper used by print and LaTeX methods.
#'
#' @param digits Number of displayed digits.
#'
#' @return A single non-negative integer.
#' @noRd
.validate_dyadic_digits <- function(digits) {
  if (
    !is.numeric(digits) ||
    length(digits) != 1L ||
    is.na(digits) ||
    !is.finite(digits) ||
    digits < 0 ||
    digits != floor(digits) ||
    digits > .Machine$integer.max
  ) {
    stop(
      "`digits` must be one non-negative integer.",
      call. = FALSE
    )
  }

  as.integer(digits)
}


#' @exportS3Method base::print
#' @noRd
print.dyadic_pattern <- function(x, ...) {
  .cat_line("Dyadic interaction pattern")

  if ("pattern" %in% names(x)) {
    .cat_line("Pattern: ", paste0(x[["pattern"]], collapse = ", "))
  }
  if ("alpha" %in% names(x)) {
    .cat_line("Alpha: ", paste0(x[["alpha"]], collapse = ", "))
  }
  if ("states" %in% names(x)) {
    .cat_line("States: ", paste0(x[["states"]], collapse = ", "))
  }

  invisible(x)
}


#' @exportS3Method base::print
#' @noRd
print.dyadic_case <- function(x, ...) {
  .cat_line("Bivariate dyadic case")

  if ("case" %in% names(x)) {
    .cat_line("Case: ", paste0(x[["case"]], collapse = ", "))
  }
  if ("alpha" %in% names(x)) {
    .cat_line("Alpha: ", paste0(x[["alpha"]], collapse = ", "))
  }

  invisible(x)
}


#' Augment an AIC table for summary output
#'
#' @param tab AIC table returned by partialPattern() or completePattern().
#' @param selected_pattern Pattern selected by the package.
#' @param tolerance Relative numerical tolerance used to identify tied minima.
#'
#' @return The augmented AIC table.
#' @noRd
.augment_dyadic_aic_table <- function(
    tab,
    selected_pattern,
    tolerance = sqrt(.Machine$double.eps)) {

  if (
    !is.data.frame(tab) ||
    !all(c("pattern", "matrix", "aic") %in% names(tab))
  ) {
    stop(
      "`tab` must contain pattern, matrix, and aic columns.",
      call. = FALSE
    )
  }

  if (!is.numeric(tab$aic)) {
    stop(
      "The `aic` column must be numeric.",
      call. = FALSE
    )
  }

  finite <- is.finite(tab$aic)

  delta_aic <- tab$aic
  minimum <- rep(FALSE, nrow(tab))

  if (any(finite)) {
    minimum_aic <- min(tab$aic[finite])

    delta_aic[finite] <-
      tab$aic[finite] - minimum_aic

    tie_tolerance <-
      tolerance * max(
        1,
        abs(minimum_aic)
      )

    minimum[finite] <-
      abs(
        tab$aic[finite] -
          minimum_aic
      ) <= tie_tolerance

  } else {
    delta_aic[] <- NA_real_
  }

  if (any(finite)) {
    selected <-
      tab$pattern == selected_pattern
  } else {
    selected <-
      rep(NA, nrow(tab))
  }

  tab$delta_aic <- delta_aic
  tab$minimum <- minimum
  tab$selected <- selected

  tab
}




#' @exportS3Method base::summary
#' @noRd
summary.dyadic_pattern <- function(object, ...) {

  if (all(c("TEST.AM", "TEST.PM") %in% names(object))) {

    alpha <- object$alpha

    tests <- data.frame(
      restriction = c(
        "AM (A2): actor-only restriction",
        "PM (A3): partner-only restriction"
      ),
      statistic = c(
        unname(object$TEST.AM$statistic),
        unname(object$TEST.PM$statistic)
      ),
      df = c(
        unname(object$TEST.AM$parameter),
        unname(object$TEST.PM$parameter)
      ),
      p_value = c(
        object$TEST.AM$p.value,
        object$TEST.PM$p.value
      )
    )

    valid_p <-
      is.finite(tests$p_value) &
      tests$p_value >= 0 &
      tests$p_value <= 1

    tests$decision <- NA_character_

    tests$decision[valid_p] <- ifelse(
      tests$p_value[valid_p] <= alpha,
      "Rejected",
      "Not rejected"
    )

    out <- list(
      pattern = object$pattern,
      tests = tests,
      alpha = alpha,
      states = object$states,
      call = object$call
    )

  } else if ("aic" %in% names(object)) {

    tab <- .augment_dyadic_aic_table(
      tab = object$aic,
      selected_pattern = object$pattern
    )

    out <- list(
      pattern = object$pattern,
      aic = tab,
      call = object$call
    )

  } else {

    fields <- c(
      "pattern",
      "alpha",
      "states",
      "call"
    )

    out <-
      object[
        intersect(
          fields,
          names(object)
        )
      ]
  }

  class(out) <-
    c(
      "summary_dyadic_pattern",
      "list"
    )

  out
}


#' @exportS3Method base::summary
#' @noRd
summary.dyadic_case <- function(object, ...) {

  alpha <- object$alpha

  tests <- data.frame(
    model = c(
      "Main-variable-only model (A1)",
      "Second-variable-only model (B1)"
    ),
    statistic = c(
      unname(object$testUnivariate$statistic),
      unname(object$testPartial$statistic)
    ),
    df = c(
      unname(object$testUnivariate$parameter),
      unname(object$testPartial$parameter)
    ),
    p_value = c(
      object$testUnivariate$p.value,
      object$testPartial$p.value
    )
  )

  valid_p <-
    is.finite(tests$p_value) &
    tests$p_value >= 0 &
    tests$p_value <= 1

  tests$decision <- NA_character_

  tests$decision[valid_p] <- ifelse(
    tests$p_value[valid_p] <= alpha,
    "Rejected",
    "Not rejected"
  )

  out <- list(
    case = object$case,
    tests = tests,
    alpha = alpha,
    call = object$call
  )

  class(out) <-
    c(
      "summary_dyadic_case",
      "list"
    )

  out
}


#' Format summary p-values
#'
#' @param x Numeric p-values.
#' @param digits Number of significant digits.
#'
#' @return Character vector.
#' @noRd
.format_dyadic_summary_pvalue <- function(
    x,
    digits) {

  out <- rep(
    "Unavailable",
    length(x)
  )

  valid <-
    is.finite(x) &
    x >= 0 &
    x <= 1

  out[valid] <- format.pval(
    x[valid],
    digits = digits,
    eps = 10^(-digits)
  )

  out
}


#' Format summary numeric values
#'
#' @param x Numeric vector.
#' @param digits Number of displayed digits.
#'
#' @return Character vector.
#' @noRd
.format_dyadic_summary_numeric <- function(
    x,
    digits) {

  out <- as.character(x)

  finite <- is.finite(x)

  out[finite] <- formatC(
    x[finite],
    format = "fg",
    digits = digits
  )

  out[is.na(x)] <- "NA"
  out[is.nan(x)] <- "NaN"

  out
}


#' @exportS3Method base::print
#' @noRd
print.summary_dyadic_pattern <- function(
    x,
    digits = 3L,
    ...) {

  digits <- .validate_dyadic_digits(digits)

  if ("tests" %in% names(x)) {

    cat("Dyadic interaction pattern summary\n")
    cat("Pattern: ", x$pattern, "\n", sep = "")
    cat("Alpha: ", x$alpha, "\n", sep = "")
    cat("States: ", x$states, "\n\n", sep = "")

    cat(
      "Likelihood-ratio comparisons evaluated with Pearson Chi-squared\n\n"
    )

    tab <- x$tests

    display <- data.frame(
      Restriction = tab$restriction,
      `Chi-squared` =
        .format_dyadic_summary_numeric(
          tab$statistic,
          digits
        ),
      df =
        .format_dyadic_summary_numeric(
          tab$df,
          digits
        ),
      `p-value` =
        .format_dyadic_summary_pvalue(
          tab$p_value,
          digits
        ),
      Decision = ifelse(
        is.na(tab$decision),
        "Unavailable",
        tab$decision
      ),
      check.names = FALSE
    )

    print(
      display,
      row.names = FALSE,
      right = FALSE
    )

  } else if ("aic" %in% names(x)) {

    cat("Dyadic interaction pattern summary\n")

    if (any(is.finite(x$aic$aic))) {
      cat(
        "Selected pattern: ",
        x$pattern,
        "\n\n",
        sep = ""
      )
    } else {
      cat(
        "Selected pattern: Unavailable\n\n"
      )
    }

    cat("AIC candidate comparison\n\n")

    tab <- x$aic

    display <- data.frame(
      Matrix = tab$matrix,
      AIC =
        .format_dyadic_summary_numeric(
          tab$aic,
          digits
        ),
      `Delta AIC` =
        .format_dyadic_summary_numeric(
          tab$delta_aic,
          digits
        ),
      Selected = ifelse(
        !is.na(tab$selected) & tab$selected,
        "Yes",
        ""
      ),
      check.names = FALSE
    )

    print(
      display,
      row.names = FALSE,
      right = FALSE
    )

    if (
      sum(
        tab$minimum,
        na.rm = TRUE
      ) > 1L
    ) {
      cat(
        "\nMultiple candidates share the minimum AIC within numerical tolerance.\n"
      )
    }

    if (!any(is.finite(tab$aic))) {
      cat(
        "\nNo finite AIC candidate is available.\n"
      )
    }

  } else {

    print(
      unclass(x)
    )
  }

  invisible(x)
}


#' @exportS3Method base::print
#' @noRd
print.summary_dyadic_case <- function(
    x,
    digits = 3L,
    ...) {

  digits <- .validate_dyadic_digits(digits)

  cat("Bivariate case summary\n")
  cat("Case: ", x$case, "\n", sep = "")
  cat("Alpha: ", x$alpha, "\n\n", sep = "")

  cat(
    "Global comparisons evaluated with Pearson Chi-squared\n\n"
  )

  tab <- x$tests

  display <- data.frame(
    Model = tab$model,
    `Chi-squared` =
      .format_dyadic_summary_numeric(
        tab$statistic,
        digits
      ),
    df =
      .format_dyadic_summary_numeric(
        tab$df,
        digits
      ),
    `p-value` =
      .format_dyadic_summary_pvalue(
        tab$p_value,
        digits
      ),
    Decision = ifelse(
      is.na(tab$decision),
      "Unavailable",
      tab$decision
    ),
    check.names = FALSE
  )

  print(
    display,
    row.names = FALSE,
    right = FALSE
  )

  invisible(x)
}


#' Format a dyadic matrix for screen output
#'
#' Internal helper used by matrix-like print methods.
#'
#' @param x Matrix-like dyadic object.
#' @param digits Number of digits to display.
#'
#' @return A character matrix preserving dimnames.
#' @noRd
.format_dyadic_matrix <- function(x, digits) {
  digits <- .validate_dyadic_digits(digits)

  out <- formatC(as.vector(x), format = "f", digits = digits)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  out
}


#' Build a summary for matrix-like dyadic objects
#'
#' Internal helper used by summary methods.
#'
#' @param object Matrix-like dyadic object.
#' @param object_type Character description of the object type.
#'
#' @return A list with class, type, dimension, and name metadata.
#' @noRd
.summary_dyadic_matrix <- function(object, object_type) {
  list(
    object_type = object_type,
    object_class = class(object),
    storage_mode = storage.mode(object),
    dimensions = dim(object),
    row_names = rownames(object),
    column_names = colnames(object)
  )
}


#' @exportS3Method base::print
#' @noRd
print.dyadic_counts <- function(x, digits = 0L, ...) {
  print(.format_dyadic_matrix(x, digits = digits), quote = FALSE, ...)
  invisible(x)
}


#' @exportS3Method base::print
#' @noRd
print.dyadic_mle <- function(x, digits = 3L, ...) {
  print(.format_dyadic_matrix(x, digits = digits), quote = FALSE, ...)
  invisible(x)
}


#' @exportS3Method base::summary
#' @noRd
summary.dyadic_counts <- function(object, ...) {
  out <- .summary_dyadic_matrix(
    object = object,
    object_type = "empirical transition counts"
  )
  out$total_count <- sum(object)
  out$row_sums <- rowSums(object)
  class(out) <- c("summary_dyadic_counts", "list")
  out
}


#' @exportS3Method base::summary
#' @noRd
summary.dyadic_mle <- function(object, ...) {
  out <- .summary_dyadic_matrix(
    object = object,
    object_type = "transition probability estimates"
  )
  out$row_sums <- rowSums(object)
  class(out) <- c("summary_dyadic_mle", "list")
  out
}


#' Escape text for LaTeX output
#'
#' Internal helper used by dyadic matrix LaTeX methods.
#'
#' @param x Character vector.
#'
#' @return Escaped character vector.
#' @noRd
.escape_dyadic_latex <- function(x) {

  x <- as.character(x)

  replacements <- c(
    "\\" = "\\textbackslash{}",
    "&" = "\\&",
    "%" = "\\%",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )

  vapply(
    x,
    function(value) {
      if (is.na(value)) {
        return(NA_character_)
      }

      chars <- strsplit(value, "", fixed = TRUE)[[1L]]
      escaped <- replacements[chars]

      missing <- is.na(escaped)
      escaped[missing] <- chars[missing]

      paste0(escaped, collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}


#' Format row labels for mathematical dyadic matrices
#'
#' Internal helper translating package dimnames to the notation used in the
#' methodological presentation of dyadic transition matrices.
#'
#' @param x Matrix-like dyadic object.
#'
#' @return Character vector of LaTeX row labels.
#' @noRd
.format_dyadic_latex_rows <- function(x) {

  labels <- rownames(x)

  if (is.null(labels)) {
    return(as.character(seq_len(nrow(x))))
  }


  # ------------------------------------------------------------------
  # Univariate:
  #
  # FM1_SM1  ->  (1,1)
  # ------------------------------------------------------------------

  univariate <- grepl(
    "^FM[0-9]+_SM[0-9]+$",
    labels
  )

  if (all(univariate)) {

    matches <- regmatches(
      labels,
      regexec(
        "^FM([0-9]+)_SM([0-9]+)$",
        labels
      )
    )

    return(
      vapply(
        matches,
        function(z) {
          sprintf(
            "(%s,%s)",
            z[2L],
            z[3L]
          )
        },
        character(1L)
      )
    )
  }


  # ------------------------------------------------------------------
  # Bivariate:
  #
  # mainFM1_mainSM1_secondFM1_secondSM2
  #
  # becomes
  #
  # (1,1)\,(1,2)
  #
  # corresponding to the previous dyadic state on the main variable
  # followed by the previous dyadic state on the second variable.
  # ------------------------------------------------------------------

  bivariate <- grepl(
    paste0(
      "^mainFM[0-9]+_mainSM[0-9]+_",
      "secondFM[0-9]+_secondSM[0-9]+$"
    ),
    labels
  )

  if (all(bivariate)) {

    matches <- regmatches(
      labels,
      regexec(
        paste0(
          "^mainFM([0-9]+)_mainSM([0-9]+)_",
          "secondFM([0-9]+)_secondSM([0-9]+)$"
        ),
        labels
      )
    )

    return(
      vapply(
        matches,
        function(z) {
          sprintf(
            "(%s,%s)\\,(%s,%s)",
            z[2L],
            z[3L],
            z[4L],
            z[5L]
          )
        },
        character(1L)
      )
    )
  }


  paste0(
    "\\mbox{",
    .escape_dyadic_latex(labels),
    "}"
  )
}


#' Format column labels for mathematical dyadic matrices
#'
#' @param x Matrix-like dyadic object.
#'
#' @return Character vector of LaTeX column labels.
#' @noRd
.format_dyadic_latex_columns <- function(x) {

  labels <- colnames(x)

  if (is.null(labels)) {
    return(as.character(seq_len(ncol(x))))
  }

  next_state <- grepl(
    "^next_[0-9]+$",
    labels
  )

  if (all(next_state)) {
    return(
      sub(
        "^next_",
        "",
        labels
      )
    )
  }

  paste0(
    "\\mbox{",
    .escape_dyadic_latex(labels),
    "}"
  )
}


#' Convert a dyadic matrix to mathematical LaTeX
#'
#' Internal helper used by the `toLatex()` methods for empirical transition
#' counts and estimated transition probabilities.
#'
#' The representation follows the transition-matrix notation used in the
#' methodological presentation: previous dyadic states are displayed to the
#' left of a parenthesized numerical matrix and possible next focal states are
#' displayed above its columns.
#'
#' @param x Matrix-like dyadic object.
#' @param symbol LaTeX symbol displayed to the left of the matrix.
#' @param digits Number of displayed decimal digits.
#'
#' @return A character vector of class `"Latex"`.
#' @noRd
.to_latex_dyadic_matrix <- function(x, symbol, digits) {

  digits <- .validate_dyadic_digits(digits)

  row_labels <- .format_dyadic_latex_rows(x)

  column_labels <- .format_dyadic_latex_columns(x)

  formatted <- formatC(
    unclass(x),
    format = "f",
    digits = digits
  )

  dim(formatted) <- dim(x)


  # ------------------------------------------------------------------
  # Shared column widths for numerical entries and column labels
  # ------------------------------------------------------------------

  column_width_reference <- vapply(
    seq_len(ncol(x)),
    function(j) {
      candidates <- c(
        column_labels[j],
        formatted[, j]
      )

      candidates[
        which.max(
          nchar(candidates)
        )
      ]
    },
    character(1)
  )

  latex_column_labels <- paste0(
    "\\phantom{",
    column_width_reference,
    "}\\llap{",
    column_labels,
    "}"
  )

  latex_formatted <- matrix(
    paste0(
      "\\phantom{",
      rep(
        column_width_reference,
        each = nrow(x)
      ),
      "}\\llap{",
      as.vector(formatted),
      "}"
    ),
    nrow = nrow(x),
    ncol = ncol(x)
  )


  # ------------------------------------------------------------------
  # Numerical matrix rows
  # ------------------------------------------------------------------

  matrix_rows <- apply(
    latex_formatted,
    1L,
    paste,
    collapse = " & "
  )


  # ------------------------------------------------------------------
  # Dynamic LaTeX alignment specification
  # ------------------------------------------------------------------

  numeric_alignment <- paste(
    rep(
      "r",
      ncol(x)
    ),
    collapse = ""
  )


  # ------------------------------------------------------------------
  # Assemble the mathematical object.
  # ------------------------------------------------------------------

  lines <- c(
    "\\begin{array}{ccc}",

    paste0(
      "  & & \\begin{array}{",
      numeric_alignment,
      "} ",
      paste(
        latex_column_labels,
        collapse = " & "
      ),
      " \\end{array} \\\\"
    ),

    paste0(
      "  ",
      symbol,
      " = &"
    ),

    "  \\begin{array}{r}",

    paste0(
      "    ",
      paste(
        row_labels,
        collapse = " \\\\\n    "
      )
    ),

    "  \\end{array} &",

    paste0(
      "  \\left(\\begin{array}{",
      numeric_alignment,
      "}"
    ),

    paste0(
      "    ",
      paste(
        matrix_rows,
        collapse = " \\\\\n    "
      )
    ),

    "  \\end{array}\\right)",

    "\\end{array}"
  )


  structure(
    lines,
    class = "Latex"
  )
}


#' @exportS3Method utils::toLatex
#' @noRd
toLatex.dyadic_counts <- function(
    object,
    digits = 0L,
    ...) {

  .to_latex_dyadic_matrix(
    x = object,
    symbol = "\\Phi",
    digits = digits
  )
}


#' @exportS3Method utils::toLatex
#' @noRd
toLatex.dyadic_mle <- function(
    object,
    digits = 3L,
    ...) {

  .to_latex_dyadic_matrix(
    x = object,
    symbol = "\\widehat{\\Lambda}",
    digits = digits
  )
}
