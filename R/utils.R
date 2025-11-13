#' Load Datasets from an HDF5 File
#'
#' This function reads datasets stored in an HDF5 file and returns them as a nested list.
#' It requires the \pkg{hdf5r} package for handling HDF5 file operations.
#'
#' @param dataset_file Character string specifying the path to the HDF5 file to be loaded.
#'
#' @details
#' The HDF5 file is opened in read-only mode using the \pkg{hdf5r} package.
#' Each top-level group (e.g., \code{"train"}, \code{"valid"}, \code{"test"}) is read as a list of arrays.
#' Within each group, all datasets (e.g., \code{"x"}, \code{"t"}, \code{"e"}) are read into memory.
#'
#' The function automatically closes the HDF5 file connection after reading all contents.
#'
#' @return A named list of groups, where each group is itself a list of datasets.
#'
#' @examples
#' \dontrun{
#' library(hdf5r)
#' datasets <- load_datasets("data/survival_data.h5")
#' names(datasets)
#' str(datasets$train)
#' }
#'
#' @import hdf5r
#' @export
load_datasets <- function(dataset_file) {
  # Open HDF5 file (read-only)
  h5file <- H5File$new(dataset_file, mode = "r")
  group_names <- names(h5file)
  datasets <- list()

  for (ds in group_names) {
    group <- h5file[[ds]]
    array_names <- names(group)
    group_list <- list()

    for (array_name in array_names) {
      data <- group[[array_name]]$read()
      # Convert vectors to column matrices
      if (is.vector(data)) {
        data <- matrix(data, ncol = 1)
      }
      group_list[[array_name]] <- data
    }

    datasets[[ds]] <- group_list
  }

  h5file$close_all()
  return(datasets)
}


#' Format Survival Dataset into a Data Frame
#'
#' This function converts a dataset (in list form) into a single data frame
#' suitable for modeling or visualization. It extracts survival times and event
#' indicators from specified columns, optionally renames a treatment column, and
#' combines all covariates and outcomes into one consolidated object.
#'
#' @param dataset A list containing survival data with elements:
#'   \describe{
#'     \item{x}{A numeric matrix or data frame of covariates (predictors).}
#'     \item{t}{A numeric matrix or data frame of survival times.}
#'     \item{e}{A numeric matrix or data frame of event indicators.}
#'   }
#' @param duration_col Integer or character index specifying which column of
#'   \code{dataset$t} contains the survival time variable.
#' @param event_col Integer or character index specifying which column of
#'   \code{dataset$e} contains the event indicator (1 = event, 0 = censored).
#' @param trt_idx Optional integer or character index specifying which column of
#'   \code{dataset$x} corresponds to a treatment indicator. If provided, that
#'   column is renamed to \code{"treat"} in the output.
#'
#' @details
#' The function binds together the covariate matrix (\code{x}), the selected
#' survival time (\code{t}), and event indicator (\code{e}) into a single data
#' frame.
#' This is particularly useful for preparing data for regression modeling,
#' plotting, or summary statistics.
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{Covariates}{All columns from \code{dataset$x}.}
#'     \item{dt}{A numeric column of survival times.}
#'     \item{censor}{A numeric column of event indicators.}
#'   }
#'
#' @examples
#' dataset <- list(
#'   x = data.frame(age = rnorm(10, 50, 10), sex = rbinom(10, 1, 0.5)),
#'   t = data.frame(time = runif(10, 1, 5)),
#'   e = data.frame(event = rbinom(10, 1, 0.7))
#' )
#'
#' df <- format_dataset_to_df(dataset, duration_col = "time", event_col = "event")
#' head(df)
#'
#' @export
format_dataset_to_df<- function(dataset, duration_col, event_col, trt_idx = NULL){
  xdf <- dataset$x
  if (!is.null(trt_idx)){
    colnames(xdf)[trt_idx] <- 'treat'
  }
  dt <- dataset$t[,duration_col]
  censor <- dataset$e[,event_col]
  cdf <- cbind(xdf, dt, censor)
  return(cdf)
}

#' Standardize Covariate Matrix
#'
#' This helper function standardizes each column of a covariate matrix by
#' centering (subtracting the mean) and scaling (dividing by the standard
#' deviation). It can also apply user-specified centering and scaling values,
#' ensuring consistency across training and testing datasets.
#'
#' @param x A numeric matrix or data frame of covariates to standardize.
#' @param offset Optional numeric vector of column means. If \code{NULL}, the
#'   means of \code{x} are computed and used.
#' @param scale Optional numeric vector of column standard deviations. If
#'   \code{NULL}, the standard deviations of \code{x} are computed and used.
#'
#' @details
#' The function computes column-wise means and standard deviations if they are
#' not provided, then standardizes \code{x} so that each column has (approximately)
#' mean 0 and standard deviation 1.
#' Columns with zero variance are scaled by 1 to avoid division by zero.
#'
#' This function is commonly used internally by \code{\link{prepare_data}} to
#' ensure consistent feature scaling across datasets.
#'
#' @return A list containing:
#'   \describe{
#'     \item{x}{The standardized covariate matrix.}
#'     \item{offset}{The column means used for centering.}
#'     \item{scale}{The column standard deviations used for scaling.}
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' x <- matrix(rnorm(12), ncol = 3)
#' result <- standardize_x(x)
#'
#'
#' # Apply the same standardization to new data
#' new_x <- matrix(rnorm(6), ncol = 3)
#' standardized_new <- standardize_x(new_x, offset = result$offset, scale = result$scale)
#' }
#' @importFrom stats sd
standardize_x <- function(x, offset = NULL, scale = NULL) {
  # Compute means and SDs if not provided
  if (is.null(offset)) offset <- colMeans(x)
  if (is.null(scale)) scale <- apply(x, 2, sd)
  # Avoid dividing by zero
  scale[scale == 0] <- 1
  # Standardize
  x_std <- sweep(x, 2, offset, "-") #subtracts mean from values
  x_std <- sweep(x_std, 2, scale, "/") #divides by sd
  return(list(x = x_std, offset = offset, scale = scale))
}
