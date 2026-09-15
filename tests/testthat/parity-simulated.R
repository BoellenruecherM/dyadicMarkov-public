# tests/testthat/parity-simulated.R
# Simulated parity checks + pattern recovery sanity checks.
# Assumes dyadicMarkov is already loaded (e.g., devtools::load_all()).

if (!identical(Sys.getenv("RUN_PARITY"), "1")) {
  stop("This is a manual parity script. Set RUN_PARITY=1 before sourcing.")
}

if (!requireNamespace("dyadicMarkov", quietly = TRUE)) {
  stop("Install/load dyadicMarkov first (e.g., devtools::load_all()).")
}


set.seed(888)

# ------------------------------------------------------------
# 0) Load NEW package (current code)
# ------------------------------------------------------------
# Assumes you run devtools::load_all() BEFORE sourcing this file.

# ------------------------------------------------------------
# 1) # Load legacy functions into an isolated environment
# ------------------------------------------------------------
legacy_env <- new.env(parent = globalenv())
sys.source("tests/testthat/legacy/FunctionsFile.R", envir = legacy_env)

# Helper compare
cmp <- function(old, new, name, tol = 1e-10) {
  comparison <- all.equal(
    unclass(old),
    unclass(new),
    tolerance = tol,
    check.attributes = FALSE
  )
  ok <- isTRUE(comparison)
  cat(sprintf("[%s] %s\n", if (ok) "OK" else "FAIL", name))
  if (!ok) {
    print(comparison)
    stop("Parity check failed: ", name)
  }
  invisible(TRUE)
}

# ============================================================
# A) Simulated parity: same random chains => old vs new
# ============================================================
s <- 2
n <- 2000
chainFM <- sample.int(s, n, replace = TRUE)
chainSM <- sample.int(s, n, replace = TRUE)

old_emp <- legacy_env$countEmp(states = s, chainFM = chainFM, chainSM = chainSM)
new_emp <- dyadicMarkov::countEmp(states = s, chainFM = chainFM, chainSM = chainSM)
cmp(old_emp, new_emp, "countEmp (simulated chains)")

old_mle <- legacy_env$mleEstimation(empirical = old_emp)
new_mle <- dyadicMarkov::mleEstimation(empirical = new_emp)
cmp(old_mle, new_mle, "mleEstimation (from empirical)")

# ============================================================
# B) Simulated bivariate parity
# ============================================================
s <- 2
n <- 1500
chainFM_V1 <- sample.int(s, n, replace = TRUE)
chainSM_V1 <- sample.int(s, n, replace = TRUE)
chainFM_V2 <- sample.int(s, n, replace = TRUE)
chainSM_V2 <- sample.int(s, n, replace = TRUE)

old_emp_bi <- legacy_env$countEmpBivariate(
  states = s,
  chainFM_V1 = chainFM_V1, chainSM_V1 = chainSM_V1,
  chainFM_V2 = chainFM_V2, chainSM_V2 = chainSM_V2
)
new_emp_bi <- dyadicMarkov::countEmpBivariate(
  states = s,
  chainFM_V1 = chainFM_V1, chainSM_V1 = chainSM_V1,
  chainFM_V2 = chainFM_V2, chainSM_V2 = chainSM_V2
)
cmp(old_emp_bi, new_emp_bi, "countEmpBivariate (simulated chains)")

if (is.function(legacy_env$bivariateCase)) {
  old_case <- legacy_env$bivariateCase(empirical = old_emp_bi, alpha = 0.05)
  new_case <- dyadicMarkov::bivariateCase(empirical = new_emp_bi, alpha = 0.05)
  old_numeric <- list(
    testUnivariate = unclass(old_case$testUnivariate)[c(
      "parameter", "statistic", "p.value"
    )],
    testPartial = unclass(old_case$testPartial)[c(
      "parameter", "statistic", "p.value"
    )],
    case = old_case$case
  )
  new_numeric <- list(
    testUnivariate = unclass(new_case$testUnivariate)[c(
      "parameter", "statistic", "p.value"
    )],
    testPartial = unclass(new_case$testPartial)[c(
      "parameter", "statistic", "p.value"
    )],
    case = new_case$case
  )
  cmp(old_numeric, new_numeric, "bivariateCase numeric outputs (alpha=0.05)")
}

# ============================================================
# C) Pattern recovery (simulate a pattern -> see if functions detect it)
#    We simulate FM_{t+1} under:
#      - AM: depends only on FM_t
#      - PM: depends only on SM_t
#    Then compare legacy vs dyadicMarkov classifications.
# ============================================================

sim_dyad_AM <- function(n = 8000, seed = 888,
                        p_FM = list(`1` = c(0.90, 0.10), `2` = c(0.15, 0.85)),
                        p_SM = list(`1` = c(0.60, 0.40), `2` = c(0.40, 0.60))) {
  set.seed(seed)
  s <- 2
  FM <- integer(n); SM <- integer(n)
  FM[1] <- 1; SM[1] <- 1
  for (t in 2:n) {
    # SM evolves on its own
    SM[t] <- sample.int(s, 1, prob = p_SM[[as.character(SM[t-1])]])
    # AM: FM depends only on FM[t-1]
    FM[t] <- sample.int(s, 1, prob = p_FM[[as.character(FM[t-1])]])
  }
  list(FM = FM, SM = SM)
}

sim_dyad_PM <- function(n = 8000, seed = 888,
                        p_FM_given_SM = list(`1` = c(0.90, 0.10), `2` = c(0.10, 0.90)),
                        p_SM = list(`1` = c(0.60, 0.40), `2` = c(0.40, 0.60))) {
  set.seed(seed)
  s <- 2
  FM <- integer(n); SM <- integer(n)
  FM[1] <- 1; SM[1] <- 1
  for (t in 2:n) {
    SM[t] <- sample.int(s, 1, prob = p_SM[[as.character(SM[t-1])]])
    # PM: FM depends only on SM[t-1]
    FM[t] <- sample.int(s, 1, prob = p_FM_given_SM[[as.character(SM[t-1])]])
  }
  list(FM = FM, SM = SM)
}

check_pattern <- function(sim, label, expected_code) {
  cat("\n=== Pattern recovery:", label, "===\n")

  new_res <- dyadicMarkov::univariatePattern(
    states = 2,
    chainFM = sim$FM,
    chainSM = sim$SM
  )

  if (!is.function(legacy_env$univariatePattern)) {
    stop("legacy_env$univariatePattern not found.")
  }

  old_res <- legacy_env$univariatePattern(
    states = 2,
    chainFM = sim$FM,
    chainSM = sim$SM,
    alpha = 0.05
  )

  extract_code <- function(x) {
    pattern <- as.character(x$pattern)[1L]
    match <- regmatches(
      pattern,
      regexpr("A[0-3]", pattern)
    )

    if (length(match) != 1L || identical(match, "")) {
      stop("Could not extract univariate pattern code from: ", pattern)
    }

    match
  }

  old_code <- extract_code(old_res)
  new_code <- extract_code(new_res)

  cat(
    "OLD code:", old_code,
    " NEW code:", new_code,
    " EXPECTED code:", expected_code,
    "\n"
  )

  if (
    !identical(old_code, new_code) ||
    !identical(new_code, expected_code)
  ) {
    stop(
      "Pattern recovery failed for ",
      label,
      ": old=",
      old_code,
      ", new=",
      new_code,
      ", expected=",
      expected_code
    )
  }

  invisible(TRUE)
}

check_pattern(sim_dyad_AM(), "AM truth", "A2")
check_pattern(sim_dyad_PM(), "PM truth", "A3")
