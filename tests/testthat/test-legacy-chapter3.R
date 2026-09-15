# tests/testthat/test-legacy-chapter3.R

testthat::test_that("legacy chapter3 runs (isolated env, no library())", {

  # Never run on CRAN, and never run in CI unless explicitly requested
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  # If the user runs it locally, require the legacy deps to be installed
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("caret")

  # ------------------------------------------------------------
  # Create a clean env that does NOT rely on attached packages
  # ------------------------------------------------------------
  env <- new.env(parent = baseenv())
  env$unit <- grid::unit

  # 1) Make dyadicMarkov functions visible to the legacy script
  ns  <- asNamespace("dyadicMarkov")
  nms <- ls(ns, all.names = TRUE)
  for (nm in nms) env[[nm]] <- get(nm, envir = ns, inherits = FALSE)

  # 2) Inject package functions into env so legacy code can call them
  #    unqualified (e.g., mutate(), ggplot(), confusionMatrix(), %>%)
  inject_pkg_exports <- function(pkg, envir) {
    exports <- getNamespaceExports(pkg)
    ns_pkg  <- asNamespace(pkg)
    for (nm in exports) {
      if (exists(nm, envir = envir, inherits = FALSE)) next
      if (!exists(nm, envir = ns_pkg, inherits = FALSE)) next
      envir[[nm]] <- get(nm, envir = ns_pkg, inherits = FALSE)
    }
    invisible(TRUE)
  }


  inject_pkg_exports("dplyr", env)
  inject_pkg_exports("ggplot2", env)
  inject_pkg_exports("caret", env)

  # 3) Provide recode() used by legacy (robust fallback)
  #    (If dplyr::recode exists it was injected above; this is a fallback.)
  if (!exists("recode", envir = env, inherits = FALSE)) {
    env$recode <- function(x, ...) {
      map <- list(...)
      y <- as.character(x)
      for (nm in names(map)) y[y == nm] <- as.character(map[[nm]])
      suppressWarnings(type.convert(y, as.is = TRUE))
    }
  }

  # 4) Intercept legacy loads: data/chap3 -> tests/testthat/legacy/data/chap3
  env$load <- function(file, ...) {
    file2 <- sub("^data/chap3/", "", file)
    path <- testthat::test_path("legacy", "data", "chap3", file2)
    if (!file.exists(path)) stop("Missing legacy data file for chapter3: ", file2)
    base::load(path, envir = parent.frame(), ...)
  }

  # ------------------------------------------------------------
  # Run legacy script
  # ------------------------------------------------------------
  set.seed(888)
  sys.source(testthat::test_path("legacy/scripts/chapter3.R"), envir = env)

  # Sanity checks that the legacy script produced expected objects
  testthat::expect_true(exists("romantic_resW",  envir = env))
  testthat::expect_true(exists("emotional_resM", envir = env))
})
