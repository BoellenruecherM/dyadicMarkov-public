# tests/testthat/parity-legacy.R
# Manual parity script: compare legacy vs new on REAL thesis datasets.
# Run:
#   Sys.setenv(RUN_PARITY="1")
#   source("tests/testthat/parity-legacy.R", local = TRUE)

if (!identical(Sys.getenv("RUN_PARITY"), "1")) {
  stop("This is a manual parity script. Set RUN_PARITY=1 before sourcing.")
}

if (!requireNamespace("dyadicMarkov", quietly = TRUE)) {
  stop("Install/load dyadicMarkov first (e.g., devtools::load_all()).")
}

source(file.path("tests", "testthat", "helper-legacy.R"), local = TRUE)
legacy <- .load_legacy()

ae <- function(x, y, label, tol = 1e-10) {
  comparison <- all.equal(
    unclass(x),
    unclass(y),
    tolerance = tol,
    check.attributes = FALSE
  )

  ok <- isTRUE(comparison)

  cat(sprintf("[%s] %s\n", if (ok) "OK" else "DIFF", label))

  if (!ok) {
    print(comparison)
    stop("Parity check failed: ", label)
  }

  invisible(TRUE)
}

cat("\n=============================\n")
cat(" dyadicMarkov: parity vs legacy (REAL DATA)\n")
cat("=============================\n\n")

## ---- Chapter 2: univariate empirical counts + MLE + pattern ----
load(system.file("extdata", "chap2", "AM.RData", package = "dyadicMarkov"))
# Use FM/SM columns if present; otherwise fall back to first two columns.
stopifnot(is.data.frame(AM))
FM <- if ("FM" %in% names(AM)) AM$FM else AM[[1]]
SM <- if ("SM" %in% names(AM)) AM$SM else AM[[2]]

states <- 2L
emp_old <- legacy$countEmp(states, FM, SM)
emp_new <- dyadicMarkov:::countEmp(FM, SM, states = states)
ae(emp_old, emp_new, "chap2 AM: countEmp")

mle_old <- legacy$mleEstimation(emp_old)
mle_new <- dyadicMarkov:::mleEstimation(emp_new)
ae(mle_old, mle_new, "chap2 AM: mleEstimation")

pat_old <- legacy$univariatePattern(states, FM, SM, alpha = 0.05)
pat_new <- dyadicMarkov::univariatePattern(FM, SM, states = states, alpha = 0.05)


canon_uni <- function(x) {
  list(
    pattern = unname(if ("pattern" %in% names(x)) x$pattern else if ("MODEL" %in% names(x)) x$MODEL else x[[1]]),
    p_AM = as.numeric(x$`TEST.AM`$p.value),
    p_PM = as.numeric(x$`TEST.PM`$p.value)
  )
}

ae(canon_uni(pat_old), canon_uni(pat_new), "chap2 AM: univariatePattern (canonical)")


## ---- Chapter 3: bivariate empirical counts + case + pattern ----
load(system.file("extdata", "chap3", "dataBivariateSingleCase.RData", package = "dyadicMarkov"))
stopifnot(is.data.frame(data))
stopifnot(all(c("XW","XM","YW","YM") %in% names(data)))

empb_old <- legacy$countEmpBivariate(states, data$XM, data$XW, data$YM, data$YW)
empb_new <- dyadicMarkov:::countEmpBivariate(data$XM, data$XW, data$YM, data$YW, states = states)
ae(empb_old, empb_new, "chap3 single: countEmpBivariate (XM|XW, YM|YW)")

case_old <- legacy$bivariateCase(empb_old, alpha = 0.05)
case_new <- dyadicMarkov::bivariateCase(empb_new, alpha = 0.05)

canon_bicase <- function(x) {
  list(
    # robust “decision label”
    case = unname(if (is.character(x)) x else if ("case" %in% names(x)) x$case else if ("MODEL" %in% names(x)) x$MODEL else x[[1]]),
    p_univariate = if (!is.null(x$testUnivariate)) as.numeric(x$testUnivariate$p.value) else NA_real_,
    p_partial    = if (!is.null(x$testPartial))    as.numeric(x$testPartial$p.value)    else NA_real_
  )
}

ae(canon_bicase(case_old), canon_bicase(case_new), "chap3 single: bivariateCase (canonical)")


# extract a single label string from case_new
case_label <- if (is.character(case_new)) case_new[1] else if ("case" %in% names(case_new)) case_new$case else as.character(case_new[[1]])

canon_pattern <- function(x) {
  selected_row <- match(x$pattern, x$aic$pattern)

  if (is.na(selected_row)) {
    stop("Selected pattern could not be matched to its AIC table.")
  }

  list(
    matrix = as.character(x$aic$matrix),
    aic = as.numeric(x$aic$aic),
    selected = as.character(x$aic$matrix[selected_row])
  )
}

if (grepl("partial", tolower(case_label))) {
  p_old <- legacy$partialPattern(empb_old)
  p_new <- dyadicMarkov::partialPattern(empb_new)
  ae(
    canon_pattern(p_old),
    canon_pattern(p_new),
    "chap3 single: partialPattern (canonical)"
  )
} else if (grepl("complete", tolower(case_label))) {
  p_old <- legacy$completePattern(empb_old)
  p_new <- dyadicMarkov::completePattern(empb_new)
  ae(
    canon_pattern(p_old),
    canon_pattern(p_new),
    "chap3 single: completePattern (canonical)"
  )
} else {
  cat("Case not recognized for pattern parity: ", case_label, "\n")
}


## ---- Chapter 6: clustering input (Prob matrix) ----
load(system.file("extdata", "chap6", "dataIllustration.RData", package = "dyadicMarkov"))
stopifnot(is.data.frame(data))

# build Prob exactly like the vignette: per dyad, both directions (FM->SM and SM->FM)
.build_prob_new <- function(dat, s) {
  tcols <- grep("^TM\\d+$", names(dat), value = TRUE)
  dyads <- sort(unique(dat$dyad))
  Prob <- matrix(NA_real_, nrow = 2L * length(dyads), ncol = 8)

  for (i in seq_along(dyads)) {
    d <- dyads[i]
    fm <- dat[dat$dyad == d & dat$members == "FM", tcols]
    sm <- dat[dat$dyad == d & dat$members == "SM", tcols]
    fm <- as.integer(fm[1, ])
    sm <- as.integer(sm[1, ])

    Prob[2L*i - 1L, ] <- c(dyadicMarkov::mleEstimation(dyadicMarkov:::countEmp(fm, sm, states = s)))
    Prob[2L*i, ]      <- c(dyadicMarkov::mleEstimation(dyadicMarkov:::countEmp(sm, fm, states = s)))
  }
  Prob
}

.build_prob_old <- function(dat, s) {
  tcols <- grep("^TM\\d+$", names(dat), value = TRUE)
  dyads <- sort(unique(dat$dyad))
  Prob <- matrix(NA_real_, nrow = 2L * length(dyads), ncol = 8)

  for (i in seq_along(dyads)) {
    d <- dyads[i]
    fm <- dat[dat$dyad == d & dat$members == "FM", tcols]
    sm <- dat[dat$dyad == d & dat$members == "SM", tcols]
    fm <- as.integer(fm[1, ])
    sm <- as.integer(sm[1, ])

    Prob[2L*i - 1L, ] <- c(legacy$mleEstimation(legacy$countEmp(s, fm, sm)))
    Prob[2L*i, ]      <- c(legacy$mleEstimation(legacy$countEmp(s, sm, fm)))
  }
  Prob
}

Prob_old <- .build_prob_old(data, states)
Prob_new <- .build_prob_new(data, states)
ae(Prob_old, Prob_new, "chap6: Prob matrix (clustering input)")


