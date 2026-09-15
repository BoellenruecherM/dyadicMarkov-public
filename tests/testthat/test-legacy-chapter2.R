test_that("legacy chapter2 runs (no edits to legacy script; legacy-data load shim)", {
  skip_on_cran()

  env <- new.env(parent = baseenv())

  # Put ALL dyadicMarkov namespace objects into env (so legacy script finds functions)
  ns  <- asNamespace("dyadicMarkov")
  nms <- ls(ns, all.names = TRUE)
  for (nm in nms) {
    env[[nm]] <- get(nm, envir = ns, inherits = FALSE)
  }

  # Intercept legacy: load("data/chap2/<file>.RData")
  env$load <- function(file, ...) {
    file2 <- sub("^data/chap2/", "", file)
    path <- testthat::test_path("legacy", "data", "chap2", file2)
    if (!file.exists(path)) stop("Missing legacy data file for chapter2: ", file2)
    base::load(path, envir = parent.frame(), ...)
  }

  set.seed(888)
  sys.source(testthat::test_path("legacy/scripts/chapter2.R"), envir = env)

  # Minimal “did it run?” checks (adjust if names differ)
  expect_true(exists("apm_resFM", envir = env))
  expect_true(exists("im3_resSM", envir = env))
})
