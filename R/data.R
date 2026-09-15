#' Synthetic univariate dyadic sequence example
#'
#' A synthetic dyadic sequence with 90 observations, designed for package
#' workflow examples.
#'
#' @format A data frame with 90 rows and 3 columns:
#' \describe{
#'   \item{time}{Index of the measurement occasion.}
#'   \item{FM}{Integer state for the first member, taking values 1 or 2.}
#'   \item{SM}{Integer state for the second member, taking values 1 or 2.}
#' }
#' @details The package workflow classifies this example as \code{PM (A3)}
#'   using \code{\link{univariatePattern}} with \code{states = 2}.
#' @docType data
#' @name dyadic_univariate_example
"dyadic_univariate_example"


#' Synthetic bivariate dyadic sequence example
#'
#' A synthetic bivariate dyadic sequence with 90 observations, designed for
#' package workflow examples.
#'
#' @format A data frame with 90 rows and 5 columns:
#' \describe{
#'   \item{time}{Index of the measurement occasion.}
#'   \item{FM_V1}{Integer variable 1 state for the first member, taking values
#'     1 or 2.}
#'   \item{SM_V1}{Integer variable 1 state for the second member, taking values
#'     1 or 2.}
#'   \item{FM_V2}{Integer variable 2 state for the first member, taking values
#'     1 or 2.}
#'   \item{SM_V2}{Integer variable 2 state for the second member, taking values
#'     1 or 2.}
#' }
#' @details The bivariate workflow classifies this example as \code{complete}
#'   using \code{\link{bivariateCase}} with \code{alpha = 0.05}. The complete
#'   bivariate pattern selected by \code{\link{completePattern}} is \code{D2}.
#' @docType data
#' @name dyadic_bivariate_example
"dyadic_bivariate_example"


#' Sensitivity-analysis simulation datasets
#'
#' Fixed archived simulation datasets used in the sensitivity-analysis vignette
#' for sequence lengths 30, 60, 90, 180, and 720. The datasets contain binary
#' bivariate dyadic sequences and are re-analysed by the current package; they
#' are not re-simulated during package use or testing.
#'
#' @format Each object is a data frame with 4,000 rows, representing 1,000 dyads
#'   observed for two members and two variables. The columns \code{TM1}, ...,
#'   \code{TMn} contain the simulated binary states for the corresponding
#'   sequence length. The remaining columns are \code{members}, \code{variable},
#'   \code{caseSimulated}, \code{dyad}, and \code{local}. The five datasets were
#'   simulated separately for sequence lengths 30, 60, 90, 180, and 720 and are
#'   therefore not nested prefixes of one another. The \code{caseSimulated}
#'   column records the interaction pattern used to simulate each member-variable
#'   sequence, whereas \code{local} records the archived classification produced
#'   by the historical analysis. Historical terminology is retained in these
#'   fixed simulation datasets; current package output uses the current
#'   scientific nomenclature.
#'
#' @aliases data_complete_30 data_complete_60 data_complete_90 data_complete_180 data_complete_720
#' @docType data
#' @name sensitivity_simulation_data
NULL
