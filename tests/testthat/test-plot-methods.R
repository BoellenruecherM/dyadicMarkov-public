make_univariate_plot_object <- function(states = 2L, n = 12L) {
  states <- as.integer(states)
  n <- as.integer(n)

  chainFM <- rep(seq_len(states), length.out = n)
  chainSM <- rep(rev(seq_len(states)), length.out = n)

  structure(
    list(
      pattern = "test pattern",
      alpha = 0.05,
      states = states,
      chainFM = as.integer(chainFM),
      chainSM = as.integer(chainSM),
      call = NULL
    ),
    class = c("dyadic_pattern", "list")
  )
}


make_bivariate_plot_empirical <- function() {
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


with_plot_pdf <- function(expr, width = 7, height = 5) {
  file <- tempfile(fileext = ".pdf")

  grDevices::pdf(
    file = file,
    width = width,
    height = height
  )

  on.exit(
    {
      if (grDevices::dev.cur() > 1L) {
        grDevices::dev.off()
      }
      unlink(file)
    },
    add = TRUE
  )

  force(expr)
}


test_that("categorical sequence runs preserve exact observation intervals", {
  runs <- dyadicMarkov:::.dyadic_sequence_runs(
    c(1L, 1L, 2L, 2L, 2L, 1L)
  )

  expect_identical(
    names(runs),
    c("left", "right", "state")
  )

  expect_equal(
    runs$left,
    c(0.5, 2.5, 5.5)
  )

  expect_equal(
    runs$right,
    c(2.5, 5.5, 6.5)
  )

  expect_identical(
    runs$state,
    c(1L, 2L, 1L)
  )
})


test_that("sequence ticks retain the first and final measurement occasions", {
  short_ticks <- dyadicMarkov:::.dyadic_sequence_ticks(6L)

  expect_true(1L %in% short_ticks)
  expect_true(6L %in% short_ticks)
  expect_false(0L %in% short_ticks)
  expect_true(all(short_ticks >= 1L & short_ticks <= 6L))

  long_ticks <- dyadicMarkov:::.dyadic_sequence_ticks(720L)

  expect_true(1L %in% long_ticks)
  expect_true(720L %in% long_ticks)
  expect_false(0L %in% long_ticks)
  expect_true(all(long_ticks >= 1L & long_ticks <= 720L))
})


test_that("univariate plot specification is correct for two states", {
  object <- make_univariate_plot_object(
    states = 2L,
    n = 12L
  )

  out <- dyadicMarkov:::.build_univariate_sequence_plot_data(
    object
  )

  expect_s3_class(
    out,
    "dyadic_sequence_plot_data"
  )

  expect_identical(
    out$type,
    "univariate_sequence"
  )

  expect_identical(
    out$occasion,
    seq_len(12L)
  )

  expect_identical(
    out$chains$chainFM,
    object$chainFM
  )

  expect_identical(
    out$chains$chainSM,
    object$chainSM
  )

  expect_identical(
    out$states,
    2L
  )

  expect_identical(
    out$state_labels,
    c("State 1", "State 2")
  )

  expect_identical(
    out$row_labels,
    c("First member", "Second member")
  )

  expect_identical(
    out$col,
    c("#0072B2", "#E69F00")
  )

  expect_identical(
    out$main,
    "Univariate dyadic sequence"
  )

  expect_identical(
    out$cex,
    1
  )

  expect_true(1L %in% out$x_ticks)
  expect_true(12L %in% out$x_ticks)
  expect_false(0L %in% out$x_ticks)
})


test_that("univariate plot specification supports more than two states", {
  object <- make_univariate_plot_object(
    states = 3L,
    n = 15L
  )

  out <- dyadicMarkov:::.build_univariate_sequence_plot_data(
    object
  )

  expect_identical(
    out$states,
    3L
  )

  expect_identical(
    out$state_labels,
    c(
      "State 1",
      "State 2",
      "State 3"
    )
  )

  expect_identical(
    out$col,
    grDevices::hcl.colors(
      n = 3L,
      palette = "Dark 3"
    )
  )

  expect_identical(
    out$chains$chainFM,
    object$chainFM
  )

  expect_identical(
    out$chains$chainSM,
    object$chainSM
  )
})


test_that("custom plot colours, title, and text scaling are preserved", {
  object <- make_univariate_plot_object(
    states = 2L,
    n = 10L
  )

  out <- dyadicMarkov:::.build_univariate_sequence_plot_data(
    object,
    col = c("black", "grey70"),
    main = "Custom sequence title",
    cex = 1.25
  )

  expect_identical(
    out$col,
    c("black", "grey70")
  )

  expect_identical(
    out$main,
    "Custom sequence title"
  )

  expect_identical(
    out$cex,
    1.25
  )
})


test_that("bivariate plot specification preserves chain order and labels", {
  empirical <- make_bivariate_plot_empirical()

  object <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  out <- dyadicMarkov:::.build_bivariate_sequence_plot_data(
    object
  )

  stored <- attr(
    empirical,
    "dyadic_sequences",
    exact = TRUE
  )

  expect_s3_class(
    out,
    "dyadic_sequence_plot_data"
  )

  expect_identical(
    out$type,
    "bivariate_sequence"
  )

  expect_identical(
    names(out$chains),
    c(
      "chainFM_V1",
      "chainSM_V1",
      "chainFM_V2",
      "chainSM_V2"
    )
  )

  expect_identical(
    out$chains$chainFM_V1,
    stored$chainFM_V1
  )

  expect_identical(
    out$chains$chainSM_V1,
    stored$chainSM_V1
  )

  expect_identical(
    out$chains$chainFM_V2,
    stored$chainFM_V2
  )

  expect_identical(
    out$chains$chainSM_V2,
    stored$chainSM_V2
  )

  expect_identical(
    out$row_labels,
    c(
      "First member, main variable",
      "Second member, main variable",
      "First member, second variable",
      "Second member, second variable"
    )
  )

  expect_identical(
    out$states,
    2L
  )

  expect_identical(
    out$state_labels,
    c("State 1", "State 2")
  )

  expect_identical(
    out$col,
    c("#0072B2", "#E69F00")
  )

  expect_identical(
    out$occasion,
    seq_along(stored$chainFM_V1)
  )
})


test_that("bivariate sequence metadata is retained and propagated", {
  empirical <- make_bivariate_plot_empirical()

  expected <- list(
    chainFM_V1 = c(1L, 2L, 1L, 2L, 2L, 1L),
    chainSM_V1 = c(2L, 1L, 2L, 1L, 1L, 2L),
    chainFM_V2 = c(1L, 1L, 2L, 2L, 1L, 2L),
    chainSM_V2 = c(2L, 2L, 1L, 1L, 2L, 1L),
    states = 2L
  )

  expect_identical(
    attr(
      empirical,
      "dyadic_sequences",
      exact = TRUE
    ),
    expected
  )

  case <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  partial <- dyadicMarkov::partialPattern(
    empirical
  )

  complete <- dyadicMarkov::completePattern(
    empirical
  )

  for (object in list(
    case,
    partial,
    complete
  )) {
    expect_identical(
      attr(
        object,
        "dyadic_sequences",
        exact = TRUE
      ),
      expected
    )
  }
})


test_that("state colours must be complete, valid, visible, and distinct", {
  validate_col <- dyadicMarkov:::.validate_dyadic_plot_col

  expect_identical(
    validate_col(
      c("black", "grey70"),
      states = 2L
    ),
    c("black", "grey70")
  )

  expect_error(
    validate_col(
      "black",
      states = 2L
    ),
    "exactly one valid colour per state"
  )

  expect_error(
    validate_col(
      c("red", "red"),
      states = 2L
    ),
    "distinct colour"
  )

  expect_error(
    validate_col(
      c("red", "#FF0000"),
      states = 2L
    ),
    "distinct colour"
  )

  expect_error(
    validate_col(
      c("red", "not-a-colour"),
      states = 2L
    ),
    "invalid colour"
  )

  expect_error(
    validate_col(
      c("red", "transparent"),
      states = 2L
    ),
    "fully transparent"
  )

  expect_error(
    validate_col(
      c(1, 2),
      states = 2L
    ),
    "exactly one valid colour per state"
  )
})


test_that("plot title and text scaling are strictly validated", {
  validate_main <- dyadicMarkov:::.validate_dyadic_plot_main
  validate_cex <- dyadicMarkov:::.validate_dyadic_plot_cex

  expect_identical(
    validate_main("Title"),
    "Title"
  )

  expect_null(
    validate_main(NULL)
  )

  expect_error(
    validate_main(c("A", "B")),
    "one character string"
  )

  expect_error(
    validate_main(NA_character_),
    "one character string"
  )

  expect_identical(
    validate_cex(1.5),
    1.5
  )

  expect_null(
    validate_cex(NULL)
  )

  for (bad in list(
    0,
    -1,
    Inf,
    NA_real_,
    c(1, 2),
    "1"
  )) {
    expect_error(
      validate_cex(bad),
      "one positive finite number"
    )
  }
})


test_that("unsupported plot arguments supplied through dots are rejected", {
  object <- make_univariate_plot_object()

  expect_error(
    plot(
      object,
      lwd = 2
    ),
    "Unsupported argument"
  )

  expect_error(
    plot(
      object,
      NULL,
      NULL,
      NULL,
      2
    ),
    "unnamed graphical arguments"
  )
})


test_that("stored sequence validation rejects malformed metadata", {
  validate_sequences <-
    dyadicMarkov:::.validate_dyadic_plot_sequences

  expect_error(
    validate_sequences(
      list(
        a = c(1L, 2L),
        b = c(1L, 2L, 1L)
      ),
      states = 2L
    ),
    "same length"
  )

  expect_error(
    validate_sequences(
      list(
        a = c(1L, 3L),
        b = c(1L, 2L)
      ),
      states = 2L
    ),
    "between 1 and `states`"
  )

  expect_error(
    validate_sequences(
      list(
        a = c(1, 1.5),
        b = c(1, 2)
      ),
      states = 2L
    ),
    "integer-valued states"
  )

  expect_error(
    validate_sequences(
      list(
        a = c(1, NA),
        b = c(1, 2)
      ),
      states = 2L
    ),
    "finite integer-valued states"
  )

  expect_error(
    validate_sequences(
      list(
        a = c(1L, 2L),
        b = c(1L, 2L)
      ),
      states = 1L
    ),
    "greater than or equal to 2"
  )
})


test_that("legacy univariate objects fail with an informative plotting error", {
  object <- make_univariate_plot_object()

  object$chainSM <- NULL

  expect_error(
    dyadicMarkov:::.build_univariate_sequence_plot_data(
      object
    ),
    "does not contain the stored sequence field"
  )
})


test_that("legacy bivariate objects fail with an informative plotting error", {
  empirical <- make_bivariate_plot_empirical()

  object <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  attr(
    object,
    "dyadic_sequences"
  ) <- NULL

  expect_error(
    plot(object),
    "does not contain the stored dyadic sequences"
  )

  partial <- dyadicMarkov::partialPattern(
    empirical
  )

  attr(
    partial,
    "dyadic_sequences"
  ) <- NULL

  expect_error(
    plot(partial),
    "does not contain stored sequences"
  )
})


test_that("malformed bivariate plotting metadata is rejected", {
  empirical <- make_bivariate_plot_empirical()

  object <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  stored <- attr(
    object,
    "dyadic_sequences",
    exact = TRUE
  )

  stored$chainFM_V2 <- NULL

  attr(
    object,
    "dyadic_sequences"
  ) <- stored

  expect_error(
    dyadicMarkov:::.build_bivariate_sequence_plot_data(
      object
    ),
    "missing: chainFM_V2"
  )

  object <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  stored <- attr(
    object,
    "dyadic_sequences",
    exact = TRUE
  )

  stored$states <- 3L

  attr(
    object,
    "dyadic_sequences"
  ) <- stored

  expect_error(
    dyadicMarkov:::.build_bivariate_sequence_plot_data(
      object
    ),
    "must use `states = 2`"
  )
})


test_that("univariate plotting succeeds on short, long, and higher-state sequences", {
  short <- make_univariate_plot_object(
    states = 2L,
    n = 30L
  )

  long <- make_univariate_plot_object(
    states = 2L,
    n = 720L
  )

  higher_state <- make_univariate_plot_object(
    states = 3L,
    n = 90L
  )

  for (object in list(
    short,
    long,
    higher_state
  )) {
    expect_error(
      with_plot_pdf(
        plot(object)
      ),
      NA
    )
  }
})


test_that("all bivariate result types produce CRAN-safe PDF output", {
  empirical <- make_bivariate_plot_empirical()

  case <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  partial <- dyadicMarkov::partialPattern(
    empirical
  )

  complete <- dyadicMarkov::completePattern(
    empirical
  )

  for (object in list(
    case,
    partial,
    complete
  )) {
    expect_error(
      with_plot_pdf(
        plot(object)
      ),
      NA
    )
  }
})


test_that("plot methods invisibly return the plotting specification", {
  object <- make_univariate_plot_object()

  visible <- with_plot_pdf(
    withVisible(
      plot(object)
    )
  )

  expect_false(
    visible$visible
  )

  expect_s3_class(
    visible$value,
    "dyadic_sequence_plot_data"
  )
})


test_that("plotting restores relevant graphical parameters after success", {
  object <- make_univariate_plot_object()

  file <- tempfile(fileext = ".pdf")

  grDevices::pdf(
    file = file,
    width = 7,
    height = 5
  )

  on.exit(
    {
      if (grDevices::dev.cur() > 1L) {
        grDevices::dev.off()
      }
      unlink(file)
    },
    add = TRUE
  )

  tracked <- c(
    "mar",
    "oma",
    "mfrow",
    "xpd",
    "cex.axis",
    "cex.lab",
    "cex.main"
  )

  before <- graphics::par(tracked)

  plot(object)

  after <- graphics::par(tracked)

  expect_equal(
    after,
    before
  )
})


test_that("renderer restores relevant graphical parameters after an error", {
  object <- make_univariate_plot_object()

  data <- dyadicMarkov:::.build_univariate_sequence_plot_data(
    object
  )

  data$col[1L] <- "not-a-colour"

  file <- tempfile(fileext = ".pdf")

  grDevices::pdf(
    file = file,
    width = 7,
    height = 5
  )

  on.exit(
    {
      if (grDevices::dev.cur() > 1L) {
        grDevices::dev.off()
      }
      unlink(file)
    },
    add = TRUE
  )

  tracked <- c(
    "mar",
    "oma",
    "mfrow",
    "xpd",
    "cex.axis",
    "cex.lab",
    "cex.main"
  )

  before <- graphics::par(tracked)

  expect_error(
    dyadicMarkov:::.draw_univariate_sequence_plot(
      data
    )
  )

  after <- graphics::par(tracked)

  expect_equal(
    after,
    before
  )
})


test_that("state identity is encoded consistently across every strip", {
  empirical <- make_bivariate_plot_empirical()

  object <- dyadicMarkov::bivariateCase(
    empirical,
    alpha = 0.05
  )

  out <- dyadicMarkov:::.build_bivariate_sequence_plot_data(
    object,
    col = c("#111111", "#DDDDDD")
  )

  expect_identical(
    out$col,
    c("#111111", "#DDDDDD")
  )

  for (chain in out$chains) {
    runs <- dyadicMarkov:::.dyadic_sequence_runs(
      chain
    )

    expect_true(
      all(
        out$col[runs$state] %in%
          c("#111111", "#DDDDDD")
      )
    )
  }
})
