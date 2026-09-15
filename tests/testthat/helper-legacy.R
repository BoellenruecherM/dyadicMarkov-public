.load_legacy <- function() {
  legacy_path <- normalizePath(
    file.path(testthat::test_path("legacy", "FunctionsFile.R")),
    winslash = "/", mustWork = TRUE
  )

  if (!file.exists(legacy_path)) {
    stop("Legacy file not found at: ", normalizePath(legacy_path, winslash = "/"))
  }

  legacy <- new.env(parent = asNamespace("stats"))  # <-- key line
  sys.source(legacy_path, envir = legacy)

  legacy
}

