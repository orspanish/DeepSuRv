
#' Compute Negative Log-Likelihood
#'
#' @param self A model-like object containing a method that returns the predicted log hazard.
#' @param E A numeric vector of event indicators (1 = event occurred, 0 = censored).
#' @param deterministic Logical; if TRUE, calculates deterministic network outputs.
#'
#' @returns A single numeric value representing the negative partial log-likelihood.
#' @examples
#' fake_self <- list(risk = function(deterministic) c(0.1, 0.2, 0.3))
#' E <- c(1, 0, 1)
#' negative_log_likelihood(fake_self, E)
#' @export
negative_log_likelihood <- function(self, E, deterministic = FALSE) {
  #translated _negative_log_likelihood in python as negative_log_likelihood
  risk <- self$risk(deterministic)
  hazard_ratio <- exp(risk)
  log_risk <- log(cumsum(hazard_ratio))
  uncensored_likelihood <- t(risk) - log_risk
  censored_likelihood <- uncensored_likelihood * E
  num_observed_events <- sum(E)
  neg_likelihood <- -sum(censored_likelihood) / num_observed_events
  return(neg_likelihood)
}

#' Map string to torch optimizer function
#'
#' Returns the corresponding torch optimizer class given a string.
#'
#' @param update_fn Character. One of "sgd", "adam", or "rmsprop".
#' @return A function or NULL if the string is unrecognized.
#' @import torch
get_optimizer_from_str <- function(update_fn) {
  switch(update_fn,
         sgd = torch::optim_sgd,
         adam = torch::optim_adam,
         rmsprop = torch::optim_rmsprop,
         NULL)
}

# Internal helper: bootstrap performance metric performs bootstrap resampling on a dataset to estimate variability.
# Not exported, only used internally.
#' @importFrom stats t.test
bootstrap_metric <- function(metric_fxn, dataset, N = 100) {
  sample_dataset <- function(dataset, sample_idx) {
    # In the original Python version, the input "dataset" is a dictionary.
    # In R, we use a named list to represent the same key–value structure. Each key corresponds to a vector.
    # sample_dataset is a helper function for bootstrap_metric()
    # dataset: named list (e.g., list(x, t, e))
    # sample_idx: integer vector of resampled indices
    sampled_dataset <- list()
    for (key in names(dataset)) {
      sampled_dataset[[key]] <- dataset[[key]][sample_idx]
    }
    return(sampled_dataset)
  }

  metrics <- numeric(N)     #translated a list "metrics" as a vector "metrics"
  size <- length(dataset$x)

  for (i in seq_len(N)) {
    resample_idx <- sample(seq_len(size), size = size, replace = TRUE)
    sampled <- sample_dataset(dataset, resample_idx)
    metrics[i] <- do.call(metric_fxn, sampled)
  }

  # Find mean and 95% confidence interval
  mean_val <- mean(metrics)
  conf_interval <- t.test(metrics, conf.level = 0.95)$conf.int

  return(list(
    mean = mean_val,
    confidence_interval = conf_interval
  ))
}

# Internal helper: calculate recommended vs anti-recommended groups.
# Used for evaluating treatment recommendations.
#' @importFrom stats median
calculate_recs_and_antirecs <- function(rec_trt, true_trt, dataset, print_metrics = TRUE) {
  if (is.numeric(true_trt)) {
    true_trt <- dataset$x[, true_trt]
  }

  trt_values <- seq_along(sort(unique(true_trt))) - 1
  true_values <- sort(unique(true_trt))
  equal_trt <- lapply(seq_along(trt_values), function(i) {
    (rec_trt == trt_values[i]) & (true_trt == true_values[i])
  })
  rec_idx <- Reduce("|", equal_trt)

  rec_t <- dataset$t[rec_idx]
  antirec_t <- dataset$t[!rec_idx]
  rec_e <- dataset$e[rec_idx]
  antirec_e <- dataset$e[!rec_idx]

  if (print_metrics) {
    cat("Printing treatment recommendation metrics\n")
    metrics <- list(
      rec_median = median(rec_t),
      antirec_median = median(antirec_t)
    )
    cat("Recommendation metrics:", metrics, "\n")
  }

  return(list(
    rec_t = rec_t,
    rec_e = rec_e,
    antirec_t = antirec_t,
    antirec_e = antirec_e
  ))
}

# Internal helper: standardize dataset features.
# Performs (x - offset) / scale on dataset$x.
standardize_dataset <- function(dataset, offset, scale) {
  norm_ds <- dataset
  norm_ds$x <- (norm_ds$x - offset) / scale
  return(norm_ds)
}

#' Standardize a Covariate Matrix
#'
#' Internal helper function that centers and scales each column of a matrix.
#' If `offset` (column means) and `scale` (column SDs) are not provided,
#' they are computed from the data.
#'
#' @param x A numeric matrix of covariates.
#' @param offset Optional numeric vector of column means to subtract. If `NULL`,
#'   column means are computed from `x`.
#' @param scale Optional numeric vector of column standard deviations to divide by.
#'   If `NULL`, column SDs are computed from `x`. Any zero SD is replaced with 1
#'   to avoid division-by-zero.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{x}}{The standardized matrix.}
#'     \item{\code{offset}}{The column means used for centering.}
#'     \item{\code{scale}}{The column SDs used for scaling.}
#'   }
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

#' Format a DeepSurv Dataset into a Data Frame
#'
#' Converts a dataset list containing \code{x}, \code{t}, and \code{e}
#' components into a single data frame with covariates, duration,
#' and event indicator columns. Optionally renames a treatment column.
#'
#' @param dataset A list containing matrices \code{x}, \code{t}, and \code{e}.
#' @param duration_col Integer; column index of the duration variable in \code{dataset$t}.
#' @param event_col Integer; column index of the event indicator in \code{dataset$e}.
#' @param trt_idx Optional integer index in \code{dataset$x} that should be renamed to \code{"treat"}.
#'
#' @return A data frame containing all covariates plus \code{dt} (duration)
#'   and \code{censor} (event indicator).
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

#' Load HDF5 Datasets into R
#'
#' Internal helper function that reads a DeepSurv-style HDF5 file containing
#' groups such as \code{"train"}, \code{"valid"}, and \code{"test"}. Each group
#' is expected to contain datasets such as \code{"x"}, \code{"t"}, and \code{"e"}.
#'
#' @param dataset_file Path to an HDF5 dataset file.
#'
#' @return A named list where each element corresponds to a dataset group,
#'   and each group contains named arrays read from the HDF5 file.
#'
#' @details
#' Uses \code{hdf5r::H5File} to open the file in read-only mode. Reads each
#' dataset fully into memory using the \code{[]}-operator.
#'
#' @import hdf5r
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
