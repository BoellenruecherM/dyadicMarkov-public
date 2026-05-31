#' Compute a chi-squared statistic
#'
#' Internal helper used to compare empirical and theoretical transition count matrices.
#'
#' @param population Theoretical transition count matrix.
#' @param empirical Empirical transition count matrix.
#'
#' @return A numeric chi-squared statistic.
#' @noRd
.chisquaredDist <- function(population, empirical) {

  # Dimension check
  if (!all(dim(population) == dim(empirical))) {
    stop(".chisquaredDist: population and empirical must have the same dimensions.")
  }

  # Counts must be non-negative
  if (any(population < 0) || any(empirical < 0)) {
    stop("population and empirical must contain non-negative counts.")
  }

  # Only compute the statistic where expected counts are strictly positive
  idx <- (population > 0)

  # Return zero when no expected count is positive
  if (!any(idx)) return(0)

  # Pearson chi-square contributions: (O - E)^2 / E, summed over valid cells
  diff <- empirical[idx] - population[idx]
  sum((diff * diff) / population[idx])
}


#' Empirical transition counts for univariate dyadic sequences
#'
#' Computes empirical transition counts for the sequences of the first and
#' second member. Rows correspond to dyadic states of the two members, and
#' columns correspond to the state of the first member at the next time point.
#'
#' @param chainFM Vector of observed states for the first member (FM).
#' @param chainSM Vector of observed states for the second member (SM).
#' @param states A single integer >= 2 giving the number of states.
#' @details Rows correspond to current dyadic states \eqn{(FM_t, SM_t)}.
#'   For general \code{states}, the row index is computed as
#'   \code{1 + states * (FM_t - 1) + (SM_t - 1)}. Columns correspond to the
#'   state of the first member at the next time point, \eqn{FM_{t+1}}.
#' @returns An integer matrix with class \code{c("dyadic_counts", "matrix", "array")},
#'   with \eqn{states^2} rows and \code{states} columns. It remains usable as an
#'   ordinary matrix.
#' @examples
#' chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
#' chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
#' countEmp(chainFM, chainSM, states = 2L)
#' @export
countEmp <- function(chainFM, chainSM, states) {

  states <- .validate_states(states)
  .validate_univariate_chains(chainFM, chainSM, states)

  # number of transitions
  n_trans <- length(chainFM) - 1L

  # current dyad state (FM_t, SM_t) and next FM state (FM_{t+1})
  bfm <- chainFM[1L:n_trans]
  bsm <- chainSM[1L:n_trans]
  col <- chainFM[2L:(n_trans + 1L)]

  # map (FM_t, SM_t) to a row index in 1:(states^2)
  row <- 1L + states * (bfm - 1L) + (bsm - 1L)

  # flatten (row, col) into a single index for tabulate()
  nrow_out <- states * states
  idx <- row + (col - 1L) * nrow_out

  # count occurrences and reshape to matrix
  tab <- tabulate(idx, nbins = nrow_out * states)
  count <- matrix(tab, nrow = nrow_out, ncol = states)
  dimnames(count) <- NULL

  # internal consistency check
  if (sum(count) != n_trans) {
    stop("internal error: empirical counts do not sum to the number of transitions.")
  }

  class(count) <- c("dyadic_counts", "matrix", "array")
  count
}


#' Maximum likelihood estimation of transition probabilities
#'
#' Estimates transition probabilities by maximum likelihood from an empirical
#' count matrix returned by \code{\link{countEmp}} or
#' \code{\link{countEmpBivariate}}.
#'
#' @param empirical An empirical count matrix.
#' @details Each row of \code{empirical} is normalized independently. Rows with
#'   zero total count are assigned a uniform probability vector, so each row of
#'   the returned matrix sums to one.
#' @returns A numeric matrix with class \code{c("dyadic_mle", "matrix", "array")}
#'   containing estimated transition probabilities with the same dimensions as
#'   \code{empirical}. It remains usable as an ordinary matrix.
#' @examples
#' chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
#' chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
#' emp <- countEmp(chainFM, chainSM, states = 2L)
#' mleEstimation(emp)
#' @export
mleEstimation <- function(empirical) {

  # Empirical counts must be supplied as a matrix
  if (!is.matrix(empirical)) {
    stop("empirical must be a matrix.", call. = FALSE)
  }

  # The matrix must contain at least one row and two columns
  if (nrow(empirical) < 1L || ncol(empirical) < 2L) {
    stop("empirical must have at least one row and at least two columns.", call. = FALSE)
  }

  # Counts should be finite, non-negative, and fully observed
  if (!is.numeric(empirical) || anyNA(empirical) ||
      any(!is.finite(empirical)) || any(empirical < 0)) {
    stop("empirical must contain finite non-negative counts with no NA.", call. = FALSE)
  }

  # Row totals: how many transitions we observed from each dyad state
  rs <- rowSums(empirical)

  # Initialize rows with uniform probabilities
  estimate <- matrix(
    1 / ncol(empirical),
    nrow = nrow(empirical),
    ncol = ncol(empirical)
  )

  # Normalize rows with observed transitions
  nz <- rs != 0
  if (any(nz)) {
    estimate[nz, ] <- empirical[nz, , drop = FALSE] / rs[nz]
  }

  class(estimate) <- c("dyadic_mle", "matrix", "array")
  estimate
}


#' Construct theoretical univariate transition counts
#'
#' Internal helper used to construct restricted theoretical count matrices for univariate patterns.
#'
#' @param empirical Empirical transition count matrix.
#' @param pattern Character string specifying the restricted pattern.
#'
#' @return A theoretical transition count matrix.
#' @noRd
countTheo <- function(empirical, pattern = c("AM", "PM")) {
  pattern <- match.arg(pattern)

  .validate_empirical_matrix(empirical)

  states <- ncol(empirical)

  # Row totals: total outgoing transitions from each dyad state
  gamma <- rowSums(empirical)

  # Row groups: dyad state (FM, SM) grouped either by actor (AM) or partner (PM)
  actor   <- rep(seq_len(states), each = states)
  partner <- rep(seq_len(states), times = states)
  gid <- if (pattern == "AM") actor else partner

  # Denominators per group (sum of row totals within each group)
  xi <- base::rowsum(gamma, gid, reorder = FALSE)[, 1]

  # Numerators per group and per column
  probs <- matrix(0, nrow = states, ncol = states)
  for (j in seq_len(states)) {
    numj <- base::rowsum(empirical[, j], gid, reorder = FALSE)[, 1]
    pj <- numj / xi
    pj[xi == 0] <- 0
    probs[, j] <- pj
  }

  # Expand group-level probabilities back to rows and scale by row totals
  out <- probs[gid, , drop = FALSE] * gamma
  out
}


#' Run a local likelihood ratio test
#'
#' Internal helper for comparing empirical and theoretical transition count matrices.
#'
#' @param population Theoretical transition count matrix.
#' @param empirical Empirical transition count matrix.
#'
#' @return An object of class `htest`.
#' @noRd
lrtLocal <- function(population, empirical) {

  # Pearson chi-square distance between observed (empirical) and expected (population)
  khi2 <- .chisquaredDist(population = population, empirical = empirical)

  # Degrees of freedom for the local approach test
  degree <- ncol(population) * (ncol(population) - 1L)^2

  # p-value (Inf statistic => p-value 0)
  pValue <- if (is.infinite(khi2)) 0 else stats::pchisq(q = khi2, df = degree, lower.tail = FALSE)

  .make_htest(
    method = "Chi-squared test",
    dataName = "Observed vs Estimated",
    alternative = "The unrestricted model fits the data better",
    statistic = khi2,
    df = degree,
    pValue = pValue
  )
}


# Precomputed group ids for bivariate constraints (states = 2 => 16 rows)
.G_GID <- list(
  A1 = rep(1L:4L, each = 4L),   # blocks of 4 rows
  B1 = rep(1L:4L, times = 4L)  # interleaved rows
)

.P_GID <- list(
  B2 = rep(c(1L, 1L, 2L, 2L), times = 4L),  # split each block into two halves
  B3 = rep(c(1L, 2L, 1L, 2L), times = 4L)   # alternating within each block
)

# Build G-type constrained theoretical count matrices (two variants)
#' Construct bivariate theoretical counts for global comparison G
#'
#' Internal helper used in the bivariate global identification step.
#'
#' @param empirical Empirical bivariate transition count matrix.
#'
#' @return A list of theoretical bivariate transition count matrices.
#' @noRd
countTheoBivariateG <- function(empirical) {
  list(
    .fill_theo_by_gid(empirical, .G_GID$A1),
    .fill_theo_by_gid(empirical, .G_GID$B1)
  )
}

# Build P-type constrained theoretical count matrices (two variants)
#' Construct bivariate theoretical counts for global comparison P
#'
#' Internal helper used in the bivariate global identification step.
#'
#' @param empirical Empirical bivariate transition count matrix.
#'
#' @return A list of theoretical bivariate transition count matrices.
#' @noRd
countTheoBivariateP <- function(empirical) {
  list(
    .fill_theo_by_gid2(empirical, .P_GID$B2),
    .fill_theo_by_gid2(empirical, .P_GID$B3)
  )
}


# Precomputed group ids for C2/C3 constraints (states = 2 => 16 rows)
.C2_GID <- list(
  E1 = c(1L,2L,1L,2L, 3L,4L,3L,4L, 1L,2L,1L,2L, 3L,4L,3L,4L),  # alternating within each half-block
  E2 = c(1L,1L,2L,2L, 3L,3L,4L,4L, 1L,1L,2L,2L, 3L,3L,4L,4L),  # pairs within each half-block
  E3 = c(
    1L,2L,1L,2L, 1L,2L,1L,2L,
    3L,4L,3L,4L, 3L,4L,3L,4L
  ),  # alternating across the two main blocks
  E4 = c(1L,1L,2L,2L, 1L,1L,2L,2L, 3L,3L,4L,4L, 3L,3L,4L,4L)   # pairs across the two main blocks
)

.C3_GID <- list(
  D1 = rep(1L:8L, times = 2L),                              # pair row i with i+8
  D2 = c(rep(1L:4L, times = 2L), rep(5L:8L, times = 2L)),  # split into two halves, each repeated
  D3 = c(
    1L,2L,1L,2L, 3L,4L,3L,4L,
    5L,6L,5L,6L, 7L,8L,7L,8L
  ),  # alternating within each 4-row block
  D4 = rep(1L:8L, each = 2L)                                 # consecutive pairs
)


# Fill a constrained theoretical count matrix using a precomputed group id (states = 2 => 16x2)
#' Fill theoretical counts by group identifier
#'
#' Internal helper for aggregating empirical bivariate counts according to a grouping structure.
#'
#' @param empirical Empirical bivariate transition count matrix.
#' @param gid Integer grouping identifier.
#'
#' @return A theoretical bivariate transition count matrix.
#' @noRd
.fill_theo_by_gid <- function(empirical, gid) {

  # Internal contract: bivariate code currently supports states = 2 only
  if (!is.matrix(empirical) || nrow(empirical) != 16L || ncol(empirical) != 2L) {
    stop(".fill_theo_by_gid expects a 16x2 empirical count matrix (states = 2).")
  }

  # Row totals (outgoing counts per row)
  rs <- rowSums(empirical)

  # Group totals (denominator) and group counts for column 1 (numerator)
  denom <- base::rowsum(rs, gid, reorder = FALSE)[, 1]
  num   <- base::rowsum(empirical[, 1], gid, reorder = FALSE)[, 1]

  # Group-level probability for column 1 (col 2 is 1 - p)
  p <- num / denom
  p[denom == 0] <- 0
  p[!is.finite(p)] <- 0

  out <- cbind(p[gid] * rs, (1 - p[gid]) * rs)
  out
}

# Faster special-case when gid has exactly 2 groups (1 and 2),
# still with states = 2 => 16x2
#' Fill theoretical counts by secondary group identifier
#'
#' Internal helper for aggregating empirical bivariate counts according to a secondary grouping structure.
#'
#' @param empirical Empirical bivariate transition count matrix.
#' @param gid Integer grouping identifier.
#'
#' @return A theoretical bivariate transition count matrix.
#' @noRd
.fill_theo_by_gid2 <- function(empirical, gid) {

  # Internal contract: bivariate code currently supports states = 2 only
  if (!is.matrix(empirical) || nrow(empirical) != 16L || ncol(empirical) != 2L) {
    stop(".fill_theo_by_gid2 expects a 16x2 empirical count matrix (states = 2).")
  }

  rs <- rowSums(empirical)
  g1 <- (gid == 1L)

  # Two group totals and two group numerators for column 1
  denom1 <- sum(rs[g1])
  denom2 <- sum(rs[!g1])
  num1 <- sum(empirical[g1, 1])
  num2 <- sum(empirical[!g1, 1])

  p1 <- if (denom1 > 0) num1 / denom1 else 0
  p2 <- if (denom2 > 0) num2 / denom2 else 0

  # Expand group-level p back to rows and scale by row totals
  p <- numeric(length(rs))
  p[g1]  <- p1
  p[!g1] <- p2

  out <- cbind(p * rs, (1 - p) * rs)
  out
}


# Build C3-type constrained theoretical count matrices (four variants)
#' Construct bivariate theoretical counts for C3-type restrictions
#'
#' Internal helper used in complete bivariate pattern comparison.
#'
#' @param empirical Empirical bivariate transition count matrix.
#'
#' @return A list of theoretical bivariate transition count matrices.
#' @noRd
countTheoBivariateC3 <- function(empirical) {
  list(
    .fill_theo_by_gid(empirical, .C3_GID$D1),
    .fill_theo_by_gid(empirical, .C3_GID$D2),
    .fill_theo_by_gid(empirical, .C3_GID$D3),
    .fill_theo_by_gid(empirical, .C3_GID$D4)
  )
}


# Build C2-type constrained theoretical count matrices (four variants)
#' Construct bivariate theoretical counts for C2-type restrictions
#'
#' Internal helper used in complete bivariate pattern comparison.
#'
#' @param empirical Empirical bivariate transition count matrix.
#'
#' @return A list of theoretical bivariate transition count matrices.
#' @noRd
countTheoBivariateC2 <- function(empirical) {
  list(
    .fill_theo_by_gid(empirical, .C2_GID$E1),
    .fill_theo_by_gid(empirical, .C2_GID$E2),
    .fill_theo_by_gid(empirical, .C2_GID$E3),
    .fill_theo_by_gid(empirical, .C2_GID$E4)
  )
}


# Chi-squared test for a constrained bivariate structure (states = 2 for now)
#' Run a bivariate likelihood ratio test
#'
#' Internal helper for testing empirical bivariate counts against a restricted theoretical matrix.
#'
#' @param population Theoretical bivariate transition count matrix.
#' @param empirical Empirical bivariate transition count matrix.
#'
#' @return An object of class `htest`.
#' @noRd
bivariateTest <- function(population, empirical) {

  # Pearson chi-square distance between observed (empirical) and expected (population)
  khi2 <- .chisquaredDist(population = population, empirical = empirical)

  # Degrees of freedom: currently derived for states = 2 (16x2 counts)
  # If the bivariate theory is generalized, then update this part accordingly.
  degree <- nrow(population) * (ncol(population) - 1L) - 4L  # 16*1 - 4 = 12

  pValue <- stats::pchisq(q = khi2, df = degree, lower.tail = FALSE)

  .make_htest(
    method = "Chi-squared test",
    dataName = "Observed vs Estimated",
    alternative = "The unrestricted model fits the data better",
    statistic = khi2,
    df = degree,
    pValue = pValue
  )
}


#' Compute AIC for bivariate pattern candidates
#'
#' Internal helper for model comparison among bivariate theoretical transition structures.
#'
#' @param population Theoretical bivariate transition count matrix.
#' @param empirical Empirical bivariate transition count matrix.
#' @param test Character string specifying the comparison type.
#'
#' @return A numeric AIC value.
#' @noRd
aicBivariate <- function(population, empirical, test = c("single", "duo", "triplet", "quadruplet")) {

  # Basic checks: same shape and valid counts
  if (!all(dim(population) == dim(empirical))) {
    stop("aicBivariate: population and empirical must have the same dimensions.")
  }
  if (any(population < 0) || any(empirical < 0) || anyNA(population) || anyNA(empirical)) {
    stop("population and empirical must contain non-negative counts with no NA.")
  }

  # Fast path for valid scalar strings, with fallback for legacy arg checking
  # behavior
  if (!(length(test) == 1L && !is.na(test) &&
        (test == "single" || test == "duo" || test == "triplet" || test == "quadruplet"))) {
    test <- match.arg(test)
  }

  # Number of free parameters for each structure
  k <- if (test == "single") 2L else if (test == "duo") 4L else if (test == "triplet") 8L else 16L

  # If empirical has mass where the model assigns zero, the likelihood is zero => AIC is infinite
  if (any(population == 0 & empirical > 0)) return(Inf)

  # G^2 = 2 * sum(emp * log(emp/pop)) over cells with emp>0 and pop>0
  idx <- (empirical > 0) & (population > 0)
  G2 <- 2 * sum(empirical[idx] * log(empirical[idx] / population[idx]))

  2 * k + G2
}


#' Compute AIC values for multiple bivariate candidates
#'
#' Internal helper for applying bivariate AIC comparison to several candidate matrices.
#'
#' @param empirical Empirical bivariate transition count matrix.
#' @param populations List of theoretical bivariate transition count matrices.
#' @param ks Numeric vector of model complexity penalties.
#'
#' @return A numeric vector of AIC values.
#' @noRd
.aicBivariate_many <- function(empirical, populations, ks) {

  # One AIC value per candidate population matrix
  if (length(populations) != length(ks)) {
    stop("populations and ks must have the same length.")
  }

  # Empirical counts must be a valid count matrix
  if (!is.matrix(empirical)) {
    stop("empirical input for AIC population comparison must be a matrix.")
  }
  if (anyNA(empirical) || any(empirical < 0)) {
    stop("empirical must contain non-negative counts with no NA.")
  }

  # Precompute where empirical counts are positive (used for every candidate)
  emp_pos <- (empirical > 0)

  out <- numeric(length(populations))
  names(out) <- names(populations)

  for (i in seq_along(populations)) {
    pop <- populations[[i]]

    # Each candidate population must match the empirical shape and be valid counts
    if (!all(dim(pop) == dim(empirical))) {
      stop("each population must have the same dimensions as empirical.")
    }
    if (anyNA(pop) || any(pop < 0)) {
      stop("population matrices must contain non-negative counts with no NA.")
    }

    # If the model assigns zero expected count where we observed positives,
    # the likelihood is zero => AIC is infinite
    if (any(pop == 0 & emp_pos)) {
      out[i] <- Inf
      next
    }

    # G^2 = 2 * sum(emp * log(emp/pop)) over cells with emp>0 and pop>0
    idx <- emp_pos & (pop > 0)
    G2 <- 2 * sum(empirical[idx] * log(empirical[idx] / pop[idx]))

    # AIC = 2k + G^2
    out[i] <- 2 * ks[i] + G2
  }

  out
}


#' Construct an htest object
#'
#' Internal helper for returning likelihood ratio test results in standard `htest` format.
#'
#' @param method Character string describing the test method.
#' @param dataName Character string naming the data.
#' @param alternative Character string describing the alternative hypothesis.
#' @param statistic Numeric test statistic.
#' @param df Numeric degrees of freedom.
#' @param pValue Numeric p-value.
#'
#' @return An object of class `htest`.
#' @noRd
.make_htest <- function(method, dataName, alternative, statistic, df, pValue) {

  # htest expects named scalar statistic and parameter
  statistic <- as.numeric(statistic)[1]
  df <- as.numeric(df)[1]

  names(statistic) <- "X-squared"
  names(df) <- "df"

  test <- list(
    method      = method,
    data.name   = dataName,
    parameter   = df,
    alternative = alternative,
    statistic   = statistic,
    p.value     = pValue
  )
  class(test) <- "htest"
  test
}
