
#' Compute Negative Log-Likelihood
#'
#' @param self A model-like object containing a method that returns the predicted log hazard.
#' @param E A numeric vector of event indicators (1 = event occurred, 0 = censored).
#' @param deterministic Logical; if TRUE, calculates deterministic network outputs.
#'
#' @returns A single numeric value representing the negative partial log-likelihood.
#' @export
#'
#' @examples
#' fake_self <- list(risk = function(deterministic) c(0.1, 0.2, 0.3))
#' E <- c(1, 0, 1)
#' negative_log_likelihood(fake_self, E)
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

#' Retrieve Optimizer Function by Name
#'
#' @param update_fn A character string specifying the optimizer name.
#'
#' @returns The corresponding optimizer function (if found), or NULL (if the name is not recognized).
#' @export
#'
#' @examples
get_optimizer_from_str <- function(update_fn) {
  if (update_fn == "sgd") {
    return(lasagne$updates$sgd)         #translated lasagne.updates.sgd as lasagne$updates$sgd
  } else if (update_fn == "adam") {
    return(lasagne$updates$adam)        #same as above
  } else if (update_fn == "rmsprop") {
    return(lasagne$updates$rmsprop)     #same
  }
  return(NULL)
}

#' Bootstrap Metric Evaluation
#'
#' @param metric_fxn A function that computes a scalar metric value from a sampled dataset.
#' @param dataset A named list containing the data vectors to resample.
#' @param N Integer; the number of bootstrap samples to draw (default = 100).
#'
#' @returns
#' A list containing:
#' {mean_val}The mean of the metric across bootstrap samples.
#' {confidence_interval}The 95\% confidence interval of the metric.
#' @export
#'
#' @examples
#' dataset <- list(x = 1:5)
#' metric_fxn <- function(x) mean(x)
#' bootstrap_metric(metric_fxn, dataset, N = 10)
bootstrap_metric <- function(metric_fxn, dataset, N = 100) {
#' Title
#'
#' @param dataset
#' @param sample_idx
#'
#' @returns
#' @export
#'
#' @examples
  sample_dataset <- function(dataset, sample_idx) {
    # In the original Python version, the input "dataset" is a dictionary.
    # In R, we use a named list to represent the same key–value structure. Each key corresponds to a vector.
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

#' Calculate Recommended and Anti-Recommended Treatment Groups
#'
#' @param rec_trt A numeric or integer vector representing the model's recommended treatment for each sample.
#' @param true_trt A numeric or integer vector giving the true treatment assignments.
#' @param dataset A named list containing elements {t}, {e}, and {x}.
#' @param print_metrics Logical; if TRUE, prints median times for recommended and anti-recommended groups.
#'
#' @returns
#' A list containing:
#' {rec_t}Times for recommended cases.
#' {rec_e}Event indicators for recommended cases.
#' {antirec_t}Times for anti-recommended cases.
#' {antirec_e}Event indicators for anti-recommended cases.
#'
#' @export
#'
#' @examples
#' dataset <- list(t = c(1, 2, 3), e = c(1, 1, 0), x = c(0, 1, 1))
#' rec_trt <- c(0, 1, 1)
#' true_trt <- c(0, 1, 0)
#' calculate_recs_and_antirecs(rec_trt, true_trt, dataset, print_metrics = FALSE)
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

#' Standardize Dataset Features
#'
#' @param dataset A named list containing the numeric vector {x} to be standardized.
#' @param offset Numeric value used to shift the data (subtract from each element).
#' @param scale Numeric value used to rescale the data (divide each element).
#'
#' @returns
#' A list with the same structure as {dataset}, but with standardized {x}.
#' @export
#'
#' @examples
#' ds <- list(x = c(2, 4, 6))
#' standardize_dataset(ds, offset = 2, scale = 2)
standardize_dataset <- function(dataset, offset, scale) {
  norm_ds <- dataset
  norm_ds$x <- (norm_ds$x - offset) / scale
  return(norm_ds)
}
