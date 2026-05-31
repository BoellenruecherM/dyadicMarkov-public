#' Print a line with newline termination
#'
#' Internal helper used by print methods.
#'
#' @param ... Objects passed to `cat()`.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
.cat_line <- function(...) {
  cat(..., "\n", sep = "")
  invisible(NULL)
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


#' @exportS3Method base::summary
#' @noRd
summary.dyadic_pattern <- function(object, ...) {
  fields <- c("pattern", "aic", "alpha", "states", "call")
  out <- object[intersect(fields, names(object))]
  class(out) <- c("summary_dyadic_pattern", "list")
  out
}


#' @exportS3Method base::summary
#' @noRd
summary.dyadic_case <- function(object, ...) {
  fields <- c("case", "alpha", "call")
  out <- object[intersect(fields, names(object))]
  class(out) <- c("summary_dyadic_case", "list")
  out
}
