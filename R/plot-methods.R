#' Validate plot text scaling
#'
#' @param cex Positive finite scalar or `NULL`.
#'
#' @return Numeric scalar or `NULL`.
#' @noRd
.validate_dyadic_plot_cex <- function(cex) {
  if (is.null(cex)) {
    return(NULL)
  }

  if (
    !is.numeric(cex) ||
    length(cex) != 1L ||
    is.na(cex) ||
    !is.finite(cex) ||
    cex <= 0
  ) {
    stop(
      "`cex` must be one positive finite number or `NULL`.",
      call. = FALSE
    )
  }

  as.numeric(cex)
}


#' Validate plot title
#'
#' @param main Character scalar or `NULL`.
#'
#' @return Character scalar or `NULL`.
#' @noRd
.validate_dyadic_plot_main <- function(main) {
  if (is.null(main)) {
    return(NULL)
  }

  if (
    !is.character(main) ||
    length(main) != 1L ||
    is.na(main)
  ) {
    stop(
      "`main` must be one character string or `NULL`.",
      call. = FALSE
    )
  }

  main
}


#' Construct accessible default state colours
#'
#' @param states Number of categorical states.
#'
#' @return Character vector with one colour per state.
#' @noRd
.dyadic_default_state_col <- function(states) {
  if (states == 2L) {
    return(c("#0072B2", "#E69F00"))
  }

  grDevices::hcl.colors(
    n = states,
    palette = "Dark 3"
  )
}


#' Validate plotting colours
#'
#' @param col State-colour vector or `NULL`.
#' @param states Number of categorical states.
#'
#' @return Character vector with one distinct, non-fully-transparent colour
#'   per state.
#' @noRd
.validate_dyadic_plot_col <- function(col, states) {
  if (is.null(col)) {
    col <- .dyadic_default_state_col(states)
  }

  if (
    !is.character(col) ||
    length(col) != states ||
    anyNA(col)
  ) {
    stop(
      "`col` must contain exactly one valid colour per state.",
      call. = FALSE
    )
  }

  rgba <- tryCatch(
    grDevices::col2rgb(col, alpha = TRUE),
    error = function(e) NULL
  )

  if (is.null(rgba)) {
    stop(
      "`col` contains an invalid colour.",
      call. = FALSE
    )
  }

  if (any(rgba[4L, ] == 0L)) {
    stop(
      "`col` must not contain fully transparent colours.",
      call. = FALSE
    )
  }

  encoded <- apply(
    rgba,
    2L,
    paste,
    collapse = ","
  )

  if (anyDuplicated(encoded)) {
    stop(
      "`col` must use a distinct colour for every state.",
      call. = FALSE
    )
  }

  col
}


#' Reject unsupported graphical arguments
#'
#' The methods retain `...` for S3 compatibility. Additional graphical
#' arguments are deliberately not silently ignored.
#'
#' @param ... Additional arguments.
#'
#' @return Invisibly `NULL`.
#' @noRd
.validate_dyadic_plot_dots <- function(...) {
  dots <- list(...)

  if (length(dots) > 0L) {
    supplied <- names(dots)

    if (is.null(supplied) || !all(nzchar(supplied))) {
      detail <- "unnamed graphical arguments"
    } else {
      detail <- paste0(
        "`",
        paste(supplied, collapse = "`, `"),
        "`"
      )
    }

    stop(
      "Unsupported argument(s) supplied through `...`: ",
      detail,
      ".",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Validate stored categorical dyadic sequences
#'
#' @param chains Named list of sequence vectors.
#' @param states Number of categorical states.
#'
#' @return A list containing validated chains, `states`, and their common length.
#' @noRd
.validate_dyadic_plot_sequences <- function(chains, states) {
  if (
    !is.numeric(states) ||
    length(states) != 1L ||
    is.na(states) ||
    !is.finite(states) ||
    states < 2 ||
    states != floor(states)
  ) {
    stop(
      "`states` stored for plotting must be one integer greater than or equal to 2.",
      call. = FALSE
    )
  }

  states <- as.integer(states)

  if (
    !is.list(chains) ||
    length(chains) == 0L ||
    !all(
      vapply(
        chains,
        function(x) is.numeric(x) && is.null(dim(x)),
        logical(1)
      )
    )
  ) {
    stop(
      "Stored dyadic sequences must be numeric vectors.",
      call. = FALSE
    )
  }

  lengths <- vapply(chains, length, integer(1))

  if (
    any(lengths == 0L) ||
    length(unique(lengths)) != 1L
  ) {
    stop(
      "Stored dyadic sequences must be non-empty and have the same length.",
      call. = FALSE
    )
  }

  valid_chain <- vapply(
    chains,
    function(chain) {
      !anyNA(chain) &&
        all(is.finite(chain)) &&
        all(chain == floor(chain)) &&
        all(chain >= 1L & chain <= states)
    },
    logical(1)
  )

  if (!all(valid_chain)) {
    stop(
      "Stored dyadic sequences must contain finite integer-valued states between 1 and `states`.",
      call. = FALSE
    )
  }

  chains <- lapply(chains, as.integer)

  list(
    chains = chains,
    states = states,
    n = lengths[[1L]]
  )
}


#' Build x-axis ticks for a dyadic sequence plot
#'
#' @param n Sequence length.
#'
#' @return Integer vector of x-axis tick locations including 1 and `n`.
#' @noRd
.dyadic_sequence_ticks <- function(n) {
  ticks <- pretty(c(1, n))

  ticks <- ticks[
    ticks >= 1 &
      ticks <= n &
      ticks == floor(ticks)
  ]

  sort(
    unique(
      c(
        1L,
        as.integer(ticks),
        as.integer(n)
      )
    )
  )
}


#' Compress a categorical sequence into observed runs
#'
#' Each observation occupies one measurement-occasion interval centred on its
#' integer occasion. Consecutive observations in the same state are represented
#' by one rectangle, reducing the size of long vector-graphics output without
#' changing the displayed sequence.
#'
#' @param chain Integer-valued categorical sequence.
#'
#' @return A data frame containing left and right interval limits and states.
#' @noRd
.dyadic_sequence_runs <- function(chain) {
  run <- rle(chain)
  right <- cumsum(run$lengths)
  left <- right - run$lengths + 1L

  data.frame(
    left = left - 0.5,
    right = right + 0.5,
    state = as.integer(run$values)
  )
}


#' Build data for a univariate dyadic sequence plot
#'
#' @param x A univariate `dyadic_pattern` object.
#' @param col State colours or `NULL`.
#' @param main Main title or `NULL`.
#' @param cex Text scaling or `NULL` for the default.
#'
#' @return A validated sequence plotting specification.
#' @noRd
.build_univariate_sequence_plot_data <- function(
    x,
    col = NULL,
    main = NULL,
    cex = NULL) {

  required <- c("chainFM", "chainSM", "states")
  missing_fields <- setdiff(required, names(x))

  if (length(missing_fields) > 0L) {
    stop(
      "This univariate `dyadic_pattern` object does not contain the stored ",
      "sequence field(s): ",
      paste(missing_fields, collapse = ", "),
      ". Recompute the object with the current version of dyadicMarkov.",
      call. = FALSE
    )
  }

  validated <- .validate_dyadic_plot_sequences(
    chains = list(
      chainFM = x$chainFM,
      chainSM = x$chainSM
    ),
    states = x$states
  )

  n <- validated$n

  col <- .validate_dyadic_plot_col(
    col = col,
    states = validated$states
  )

  main <- .validate_dyadic_plot_main(main)

  if (is.null(main)) {
    main <- "Univariate dyadic sequence"
  }

  cex <- .validate_dyadic_plot_cex(cex)

  if (is.null(cex)) {
    cex <- 1
  }

  structure(
    list(
      type = "univariate_sequence",
      occasion = seq_len(n),
      chains = validated$chains,
      states = validated$states,
      state_labels = paste(
        "State",
        seq_len(validated$states)
      ),
      x_ticks = .dyadic_sequence_ticks(n),
      col = col,
      main = main,
      cex = cex,
      row_labels = c(
        "First member",
        "Second member"
      )
    ),
    class = c(
      "dyadic_sequence_plot_data",
      "dyadic_plot_data"
    )
  )
}


#' Build data for a bivariate dyadic sequence plot
#'
#' @param x A bivariate `dyadic_case` or `dyadic_pattern` object.
#' @param col State colours or `NULL`.
#' @param main Main title or `NULL`.
#' @param cex Text scaling or `NULL` for the default.
#'
#' @return A validated sequence plotting specification.
#' @noRd
.build_bivariate_sequence_plot_data <- function(
    x,
    col = NULL,
    main = NULL,
    cex = NULL) {

  sequences <- attr(
    x,
    "dyadic_sequences",
    exact = TRUE
  )

  if (is.null(sequences)) {
    stop(
      "This bivariate result does not contain the stored dyadic sequences ",
      "required for plotting. Recompute the empirical counts with ",
      "`countEmpBivariate()` and then recompute the bivariate result.",
      call. = FALSE
    )
  }

  required <- c(
    "chainFM_V1",
    "chainSM_V1",
    "chainFM_V2",
    "chainSM_V2"
  )

  missing_fields <- setdiff(
    required,
    names(sequences)
  )

  if (length(missing_fields) > 0L) {
    stop(
      "Stored bivariate sequence metadata is missing: ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  stored_states <- if (is.null(sequences$states)) {
    2L
  } else {
    sequences$states
  }

  valid_stored_states <- is.numeric(stored_states) &&
    length(stored_states) == 1L &&
    !is.na(stored_states) &&
    is.finite(stored_states) &&
    stored_states == 2L

  if (!valid_stored_states) {
    stop(
      "Stored bivariate sequence metadata must use `states = 2`.",
      call. = FALSE
    )
  }

  validated <- .validate_dyadic_plot_sequences(
    chains = sequences[required],
    states = stored_states
  )

  n <- validated$n

  col <- .validate_dyadic_plot_col(
    col = col,
    states = validated$states
  )

  main <- .validate_dyadic_plot_main(main)

  if (is.null(main)) {
    main <- "Bivariate dyadic sequence"
  }

  cex <- .validate_dyadic_plot_cex(cex)

  if (is.null(cex)) {
    cex <- 1
  }

  structure(
    list(
      type = "bivariate_sequence",
      occasion = seq_len(n),
      chains = validated$chains,
      states = validated$states,
      state_labels = c(
        "State 1",
        "State 2"
      ),
      x_ticks = .dyadic_sequence_ticks(n),
      col = col,
      main = main,
      cex = cex,
      row_labels = c(
        "First member, main variable",
        "Second member, main variable",
        "First member, second variable",
        "Second member, second variable"
      )
    ),
    class = c(
      "dyadic_sequence_plot_data",
      "dyadic_plot_data"
    )
  )
}


#' Draw categorical dyadic state strips
#'
#' A strip is a horizontal sequence of adjacent rectangles. Each rectangle
#' represents one observed measurement occasion, and its fill identifies the
#' categorical state. Adjacent observations in the same state are drawn as one
#' continuous run. Vertical position identifies the member and, for bivariate
#' results, the variable. No interpolation or fitted values are displayed.
#'
#' @param data Validated sequence plotting specification.
#'
#' @return The plotting specification, invisibly.
#' @noRd
.draw_dyadic_state_strips <- function(data) {
  old_par <- graphics::par(
    c(
      "mar",
      "cex.axis",
      "cex.lab"
    )
  )

  on.exit(
    graphics::par(old_par),
    add = TRUE
  )

  row_count <- length(data$chains)
  row_at <- rev(seq_len(row_count))
  n <- length(data$occasion)

  # Margin required by the actual row labels.
  left_margin <- max(
    7,
    max(
      graphics::strwidth(
        data$row_labels,
        units = "inches",
        cex = data$cex
      )
    ) / graphics::par("csi") + 1.5
  )

  # Keep one legend row for small state spaces and use two balanced
  # rows for larger state spaces.
  legend_columns <- if (data$states <= 5L) {
    data$states
  } else {
    ceiling(data$states / 2L)
  }

  legend_rows <-
    ceiling(
      data$states / legend_columns
    )

  # legend() fills entries column-wise. Reorder entries so multi-row
  # legends are visually read as:
  #
  # State 1 State 2 ... State 5
  # State 6 State 7 ... State 10
  #
  legend_order <- as.vector(
    matrix(
      seq_len(
        legend_rows * legend_columns
      ),
      nrow = legend_rows,
      ncol = legend_columns,
      byrow = TRUE
    )
  )

  legend_order <-
    legend_order[
      legend_order <= data$states
    ]

  # Enough room for the legend, but substantially less unused space
  # than in the previous version.
  top_space <-
    0.62 +
    0.20 * (legend_rows - 1L)

  graphics::par(
    mar = c(
      4,
      left_margin,
      4.4,
      1
    ),
    cex.axis = data$cex,
    cex.lab = data$cex
  )

  graphics::plot.new()

  graphics::plot.window(
    xlim = c(
      0.5,
      n + 0.5
    ),
    ylim = c(
      0.5,
      max(row_at) + top_space
    ),
    xaxs = "i",
    yaxs = "i"
  )

  # Vertical reference lines are restricted to the strip region.
  graphics::segments(
    x0 = data$x_ticks,
    y0 = min(row_at) - 0.45,
    x1 = data$x_ticks,
    y1 = max(row_at) + 0.45,
    col = "#E1E1E1",
    lwd = 0.7
  )

  # Separate the two variables in the bivariate display.
  if (row_count == 4L) {
    graphics::abline(
      h = 2.5,
      col = "#D0D0D0",
      lwd = 0.8
    )
  }

  for (i in seq_along(data$chains)) {
    runs <-
      .dyadic_sequence_runs(
        data$chains[[i]]
      )

    graphics::rect(
      xleft = runs$left,
      ybottom = row_at[i] - 0.32,
      xright = runs$right,
      ytop = row_at[i] + 0.32,
      col = data$col[runs$state],
      border = NA
    )

    graphics::rect(
      xleft = 0.5,
      ybottom = row_at[i] - 0.32,
      xright = n + 0.5,
      ytop = row_at[i] + 0.32,
      border = "#333333",
      lwd = 0.8
    )
  }

  graphics::axis(
    side = 1,
    at = data$x_ticks,
    labels = data$x_ticks,
    tck = -0.02
  )

  graphics::axis(
    side = 2,
    at = row_at,
    labels = data$row_labels,
    las = 1,
    tick = FALSE,
    line = 0
  )

  graphics::mtext(
    "t",
    side = 1,
    line = 2.5,
    cex = data$cex
  )

  graphics::title(
    main = data$main,
    line = 2,
    font.main = 2,
    cex.main = 1.5 * data$cex
  )

  # Keep a constant physical gap between the legend and the highest strip.
  strip_top <- max(row_at) + 0.32

  legend_gap <- diff(
    graphics::grconvertY(
      c(0, 0.12),
      from = "inches",
      to = "user"
    )
  )

  usr <- graphics::par("usr")

  legend_x <- mean(
    usr[1:2]
  )

  legend_y <- strip_top + legend_gap

  graphics::legend(
    x = legend_x,
    y = legend_y,
    legend =
      data$state_labels[
        legend_order
      ],
    fill =
      data$col[
        legend_order
      ],
    border = "#333333",
    ncol = legend_columns,
    bty = "n",
    cex = 1.05 * data$cex,
    xjust = 0.5,
    yjust = 0,
    xpd = NA
  )

  invisible(data)
}


#' Draw a univariate categorical dyadic sequence
#'
#' @param data Validated univariate sequence plotting specification.
#'
#' @return The plotting specification, invisibly.
#' @noRd
.draw_univariate_sequence_plot <- function(data) {
  .draw_dyadic_state_strips(data)
}


#' Draw a bivariate categorical dyadic sequence
#'
#' @param data Validated bivariate sequence plotting specification.
#'
#' @return The plotting specification, invisibly.
#' @noRd
.draw_bivariate_sequence_plot <- function(data) {
  .draw_dyadic_state_strips(data)
}


#' Plot dyadic categorical state strips
#'
#' Displays each categorical sequence retained by a dyadicMarkov result as a
#' horizontal state strip. A strip consists of adjacent coloured intervals:
#' each interval represents one measurement occasion, and its colour identifies
#' the observed state. Consecutive occasions in the same state therefore form a
#' continuous run. Vertical position identifies the member and, for bivariate
#' results, the variable.
#'
#' Univariate results contain two strips, one for each member, and support any
#' integer \eqn{\mathrm{states} \ge 2}. Bivariate results contain four strips for
#' the two members on the main and second variables; the currently developed bivariate
#' method supports `states = 2` only. The plot displays observed sequences, not
#' fitted probabilities or inferred dependency structures. Statistical
#' identification results remain available through `print()` and `summary()`.
#'
#' @param x A `dyadic_pattern` or `dyadic_case` object.
#' @param col Character vector containing exactly one distinct,
#'   non-fully-transparent colour per state, or `NULL` for an accessible
#'   default palette. Colours identify states consistently across every strip.
#' @param main Optional main title. If `NULL`, a title appropriate to the
#'   univariate or bivariate sequence is used.
#' @param cex Optional positive text scaling factor. If `NULL`, the default is
#'   `1`.
#' @param ... Additional arguments are not currently supported.
#'
#' @return Invisibly returns the validated plotting specification used to draw
#'   the state strips.
#' @references
#' Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V. (2016).
#' Visualization of categorical longitudinal and time series data.
#' *Methods Report RTI Press*, 2016.
#' \doi{10.3768/rtipress.2016.mr.0033.1602}.
#' @examples
#' chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
#' chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
#' univariate <- univariatePattern(
#'   chainFM,
#'   chainSM,
#'   states = 2L
#' )
#' plot(univariate)
#'
#' chainFM_V2 <- c(1L, 1L, 2L, 2L, 1L, 2L)
#' chainSM_V2 <- c(2L, 2L, 1L, 1L, 2L, 1L)
#' empirical <- countEmpBivariate(
#'   chainFM,
#'   chainSM,
#'   chainFM_V2,
#'   chainSM_V2,
#'   states = 2L
#' )
#' bivariate <- bivariateCase(empirical)
#' plot(bivariate)
#' @exportS3Method base::plot
plot.dyadic_pattern <- function(
    x,
    col = NULL,
    main = NULL,
    cex = NULL,
    ...) {

  .validate_dyadic_plot_dots(...)

  is_univariate <- all(
    c(
      "chainFM",
      "chainSM",
      "states"
    ) %in% names(x)
  )

  if (is_univariate) {
    data <- .build_univariate_sequence_plot_data(
      x = x,
      col = col,
      main = main,
      cex = cex
    )

    return(
      invisible(
        .draw_univariate_sequence_plot(data)
      )
    )
  }

  if (!is.null(attr(x, "dyadic_sequences", exact = TRUE))) {
    data <- .build_bivariate_sequence_plot_data(
      x = x,
      col = col,
      main = main,
      cex = cex
    )

    return(
      invisible(
        .draw_bivariate_sequence_plot(data)
      )
    )
  }

  stop(
    "This `dyadic_pattern` object does not contain stored sequences for ",
    "plotting. Recompute the result with the current version of dyadicMarkov.",
    call. = FALSE
  )
}


#' @rdname plot.dyadic_pattern
#' @exportS3Method base::plot
plot.dyadic_case <- function(
    x,
    col = NULL,
    main = NULL,
    cex = NULL,
    ...) {

  .validate_dyadic_plot_dots(...)

  data <- .build_bivariate_sequence_plot_data(
    x = x,
    col = col,
    main = main,
    cex = cex
  )

  invisible(
    .draw_bivariate_sequence_plot(data)
  )
}
