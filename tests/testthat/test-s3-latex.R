test_that("dyadic count matrices have a mathematical LaTeX representation", {

  x <- matrix(
    c(
      26L, 5L, 12L, 7L,
      4L, 14L, 5L, 16L
    ),
    nrow = 4L,
    ncol = 2L,
    dimnames = list(
      c(
        "FM1_SM1",
        "FM1_SM2",
        "FM2_SM1",
        "FM2_SM2"
      ),
      c(
        "next_1",
        "next_2"
      )
    )
  )

  class(x) <- c(
    "dyadic_counts",
    "matrix",
    "array"
  )

  out <- utils::toLatex(x)

  expect_s3_class(
    out,
    "Latex"
  )

  text <- paste(
    as.character(out),
    collapse = "\n"
  )

  expect_match(
    text,
    "\\Phi =",
    fixed = TRUE
  )

  expect_match(
    text,
    "(1,1)",
    fixed = TRUE
  )

  expect_match(
    text,
    "(2,2)",
    fixed = TRUE
  )

  expect_match(
    text,
    "\\phantom{26}\\llap{1} & \\phantom{14}\\llap{2}",
    fixed = TRUE
  )

  expect_match(
    text,
    "\\phantom{26}\\llap{26} & \\phantom{14}\\llap{4}",
    fixed = TRUE
  )
})


test_that("dyadic MLE matrices use estimated-transition notation and digits", {

  x <- matrix(
    c(
      0.5, 0.25, 0.75, 0.1,
      0.5, 0.75, 0.25, 0.9
    ),
    nrow = 4L,
    ncol = 2L,
    dimnames = list(
      c(
        "FM1_SM1",
        "FM1_SM2",
        "FM2_SM1",
        "FM2_SM2"
      ),
      c(
        "next_1",
        "next_2"
      )
    )
  )

  class(x) <- c(
    "dyadic_mle",
    "matrix",
    "array"
  )

  out <- utils::toLatex(
    x,
    digits = 2L
  )

  expect_s3_class(
    out,
    "Latex"
  )

  text <- paste(
    as.character(out),
    collapse = "\n"
  )

  expect_match(
    text,
    "\\widehat{\\Lambda} =",
    fixed = TRUE
  )

  expect_match(
    text,
    "0.50",
    fixed = TRUE
  )

  expect_match(
    text,
    "0.25",
    fixed = TRUE
  )
})


test_that("bivariate dyadic row labels are represented mathematically", {

  x <- matrix(
    c(7L, 0L),
    nrow = 1L,
    ncol = 2L,
    dimnames = list(
      "mainFM1_mainSM2_secondFM2_secondSM1",
      c(
        "next_1",
        "next_2"
      )
    )
  )

  class(x) <- c(
    "dyadic_counts",
    "matrix",
    "array"
  )

  text <- paste(
    as.character(
      utils::toLatex(x)
    ),
    collapse = "\n"
  )

  expect_match(
    text,
    "(1,2)\\,(2,1)",
    fixed = TRUE
  )

  expect_match(
    text,
    "\\phantom{1}\\llap{7} & \\phantom{2}\\llap{0}",
    fixed = TRUE
  )
})


test_that("dyadic LaTeX methods validate digits", {

  x <- matrix(
    c(
      1L, 0L,
      0L, 1L
    ),
    nrow = 2L,
    dimnames = list(
      c(
        "FM1_SM1",
        "FM1_SM2"
      ),
      c(
        "next_1",
        "next_2"
      )
    )
  )

  class(x) <- c(
    "dyadic_counts",
    "matrix",
    "array"
  )

  for (bad in list(
    -1,
    1.5,
    NA_real_,
    c(2, 3)
  )) {
    expect_error(
      utils::toLatex(
        x,
        digits = bad
      ),
      "`digits` must be one non-negative integer.",
      fixed = TRUE
    )
  }
})


test_that("LaTeX escaping handles backslashes and special characters", {
  expect_identical(
    dyadicMarkov:::.escape_dyadic_latex("a\\b%_{}&#$^~"),
    paste0(
      "a\\textbackslash{}b\\%\\_\\{\\}\\&\\#\\$",
      "\\textasciicircum{}\\textasciitilde{}"
    )
  )
})


test_that("LaTeX fallback labels are safely rendered in text mode", {
  x <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(
      c("row a^b", "row c~d"),
      c("next\\one", "next two")
    )
  )

  fit <- mleEstimation(x)
  latex <- paste(
    as.character(utils::toLatex(fit)),
    collapse = "\n"
  )

  expect_match(
    latex,
    "\\mbox{row a\\textasciicircum{}b}",
    fixed = TRUE
  )

  expect_match(
    latex,
    "\\mbox{row c\\textasciitilde{}d}",
    fixed = TRUE
  )

  expect_match(
    latex,
    "\\mbox{next\\textbackslash{}one}",
    fixed = TRUE
  )

  expect_match(
    latex,
    "\\mbox{next two}",
    fixed = TRUE
  )
})
