#' Prepare Dataset for Survival Analysis
#'
#' This function prepares a dataset for survival modeling by optionally standardizing
#' covariates and sorting the data by survival time in descending order. It is useful
#' for preprocessing before fitting models such as Cox proportional hazards or DeepSurv.
#'
#' @param dataset A list containing the survival data with elements:
#'   \describe{
#'     \item{x}{A numeric matrix or data frame of covariates (predictors).}
#'     \item{e}{A numeric or integer vector of event indicators (1 = event, 0 = censored).}
#'     \item{t}{A numeric vector of survival times.}
#'   }
#' @param standardize Logical; if \code{TRUE} (default), the covariates in \code{x} are standardized.
#' @param offset Optional numeric vector giving the means of the covariates used for standardization.
#'   If \code{NULL}, they are computed from \code{x}.
#' @param scale Optional numeric vector giving the standard deviations of the covariates used for
#'   standardization. If \code{NULL}, they are computed from \code{x}.
#'
#' @details
#' If \code{standardize = TRUE}, the function calls \code{standardize_x()} to center and scale
#' each column of \code{x}. The function then sorts all observations by survival time
#' (\code{t}) in decreasing order to ensure correct ordering for likelihood-based methods.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{x}{The (optionally) standardized and sorted covariate matrix.}
#'     \item{e}{The sorted event indicator vector.}
#'     \item{t}{The sorted survival times.}
#'     \item{offset}{The mean values used for standardization.}
#'     \item{scale}{The standard deviations used for standardization.}
#'   }
#'
#' @seealso \code{\link{standardize_x}} for the helper function used in standardization.
#'
#' @examples
#' dataset <- list(
#'   x = matrix(rnorm(20), ncol = 2),
#'   e = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1),
#'   t = runif(10, 1, 10)
#' )
#' result <- prepare_data(dataset)
#' str(result)
#'
#' @export
prepare_data <- function(dataset, standardize = TRUE, offset = NULL, scale = NULL) {
  x <- dataset$x
  e <- dataset$e
  t <- dataset$t
  # Optional - standardize
  if (standardize) {
    std <- standardize_x(x, offset, scale)
    x <- std$x; offset <- std$offset; scale <- std$scale
  }
  # Sort (descending)
  sort_idx <- order(t, decreasing = TRUE)
  x <- x[sort_idx, , drop = FALSE]
  e <- e[sort_idx]
  t <- t[sort_idx]
  # Return standardized + sorted data
  return(list(x = x, e = e, t = t, offset = offset, scale = scale))
}
