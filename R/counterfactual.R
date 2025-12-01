# Null-coalescing helper: return b if a is NULL, otherwise return a
`%||%` <- function(a, b) if (is.null(a)) b else a
# Reorder x to match expl$data column names
.cols_like <- function(expl, x) x[, colnames(expl$data), drop = FALSE]

# -------- Part 1: prototype (feature influence + single-feature CF) --------

#' Identify top influential features for an individual prediction
#'
#' Perturbs each feature slightly and measures the change in predicted risk.
#' Ranks features by their absolute effect on the output.
#'
#' @param expl A Survex explainer object created by make_deepsurv_explainer().
#' @param x_row A one-row data frame representing a single individual.
#' @param k Integer; number of top features to return (default = 3).
#' @param eps Numeric; small relative change applied to each feature (default = 0.05).
#'
#' @return A data frame containing the top influential features and their absolute risk changes.
#' @examples
#' # cf_top_features(expl, patient_data[1, ])
#' @export
cf_top_features <- function(expl, x_row, k = 3, eps = 0.05) {
  x_row <- .cols_like(expl, x_row)  #Align column names
  r0 <- as.numeric(expl$predict_function(x_row))  #Compute the original risk
  feats <- colnames(expl$data)

  #Increase each feature by eps and see which causes the largest change in risk
  delta <- vapply(feats, function(f) {
    x1 <- x_row; x1[[f]] <- x1[[f]] * (1 + eps)
    r1 <- as.numeric(expl$predict_function(x1))
    abs(r1 - r0)
  }, numeric(1))

  out <- data.frame(feature = feats, delta_abs_risk = delta, row.names = NULL)
  out_sorted <- out[order(out$delta_abs_risk, decreasing = TRUE), ]
  out_sorted[1:min(k, nrow(out)), ]
}

#' Compare original and counterfactual survival for one feature
#'
#' Generates a counterfactual scenario by modifying a single feature (e.g., -10%)
#' and recomputing the survival curve and risk score.
#'
#' @param expl A Survex explainer object.
#' @param x_row A one-row data frame of covariates.
#' @param feature Character; the feature name to modify.
#' @param change Numeric; relative change (e.g., -0.10 = -10%).
#'
#' @return A list containing:
#' \describe{
#'   \item{times}{Vector of time points.}
#'   \item{S_orig}{Original survival curve.}
#'   \item{S_cf}{Counterfactual survival curve.}
#'   \item{risk_orig}{Original risk score.}
#'   \item{risk_cf}{Counterfactual risk score.}
#'   \item{x_cf}{Modified data frame after change.}
#' }
#' @examples
#' # cf_compare_one(expl, patient_data[1, ], "cholesterol", change = -0.1)
#' @export
cf_compare_one <- function(expl, x_row, feature, change = -0.10) {
  x_row <- .cols_like(expl, x_row)
  S0 <- expl$predict_survival_function(x_row)  #original survival curve
  r0 <- as.numeric(expl$predict_function(x_row))  #original risk score

  x1 <- x_row
  x1[[feature]] <- x1[[feature]] * (1 + change)
  S1 <- expl$predict_survival_function(x1)  #new survival curve
  r1 <- as.numeric(expl$predict_function(x1))  #new risk score

  list(times = expl$times %||% seq_len(ncol(S0)),
       S_orig = as.numeric(S0[1, ]), S_cf = as.numeric(S1[1, ]),
       risk_orig = r0, risk_cf = r1, x_cf = x1)
}

#' Plot original vs. counterfactual survival curves
#'
#' Simple visualization comparing the original and counterfactual survival curves.
#'
#' @param cf_out A list returned by cf_compare_one().
#' @param main Character; plot title.
#'
#' @return A base R plot displaying survival curves.
#' @examples
#' # cf_plot(cf_result)
#' @export
cf_plot <- function(cf_out, main = "Counterfactual") {
  t <- cf_out$times
  plot(t, cf_out$S_orig, type = "l", xlab = "Time", ylab = "Survival", main = main, lwd = 2)
  lines(t, cf_out$S_cf, lwd = 2, lty = 2)
  legend("topright",
         legend = c(sprintf("orig (risk=%.3f)", cf_out$risk_orig),
                    sprintf("cf   (risk=%.3f)", cf_out$risk_cf)),
         lwd = 2, lty = c(1, 2), bty = "n")
}

# -------------- Part 2: systematic perturbation (constraints) -----------

#' Apply biological constraints to feature values
#'
#' Ensures feature values remain within user-defined min/max ranges to maintain biological plausibility.
#'
#' @param x_row A one-row data frame of covariates.
#' @param constraints A named list, where each element is itself a list
#'   possibly containing min and/or max values (e.g.list(age = list(min = 0, max = 120))).
#'
#' @return A constrained data frame with adjusted feature values.
#' @examples
#' # cf_apply_constraints(patient, list(age = list(min = 0, max = 100)))
#' @export
cf_apply_constraints <- function(x_row, constraints = NULL) {
  if (is.null(constraints)) return(x_row)
  for (nm in names(constraints)) {
    if (!nm %in% colnames(x_row)) next
    # Retrieve the user-defined constraints corresponding to feature nm
    r <- constraints[[nm]]
    if (!is.null(r$min)) x_row[[nm]] <- pmax(x_row[[nm]], r$min)
    if (!is.null(r$max)) x_row[[nm]] <- pmin(x_row[[nm]], r$max)
  }
  x_row
}

#' Apply multi-feature relative perturbations
#'
#' Modifies multiple features by given relative changes (e.g., +0.1 = +10%) and reapplies constraints.
#'
#' @param x_row A one-row data frame of covariates.
#' @param changes A named numeric vector of relative changes (e.g., c(age = -0.1, cholesterol = -0.2)).
#' @param constraints Optional constraints list passed to cf_apply_constraints().
#'
#' @return A modified data frame with updated feature values.
#' @examples
#' # cf_perturb_multi(patient, c(age = -0.1, bmi = 0.05))
#' @export
cf_perturb_multi <- function(x_row, changes, constraints = NULL) {
  for (nm in names(changes)) {
    # To ensure that the user-specified feature exists in the input data
    if (nm %in% colnames(x_row)) {
      x_row[[nm]] <- x_row[[nm]] * (1 + changes[[nm]])
    }
  }
  # Reapply constraints to keep values within valid ranges
  cf_apply_constraints(x_row, constraints)
}

# ------------- Part 3: batch CF curves + stability check -----------

#' Generate counterfactual survival curves for multiple scenarios
#'
#' Compute survival curves and risk scores under multiple counterfactual scenarios.
#' Each scenario represents a set of feature perturbations.
#'
#' @param expl A Survex explainer object created by make_deepsurv_explainer().
#' @param x_row A one-row data frame of covariates representing a single individual.
#' @param change_list A named list, where each element is a named numeric vector of
#'   relative changes for selected features (e.g., list(age = -0.1, bmi = 0.05)).
#' @param constraints Optional named list specifying min/max bounds, passed to cf_apply_constraints().
#'
#' @return A named list where each element contains:
#'   \describe{
#'     \item{times}{Vector of time points used for survival curves.}
#'     \item{S_orig}{Original survival curve.}
#'     \item{S_cf}{Counterfactual survival curve.}
#'     \item{risk_orig}{Original risk score.}
#'     \item{risk_cf}{Counterfactual risk score.}
#'     \item{x_cf}{Modified covariate data after perturbation.}
#'   }
#'
#' @examples
#' # cf_generate_curves(expl, patient[1, ],
#' #   list(low_bp = c(bp = -0.2), high_bmi = c(bmi = 0.1)))
#' @export
cf_generate_curves <- function(expl, x_row, change_list, constraints = NULL) {
  x_row <- .cols_like(expl, x_row)
  S0 <- expl$predict_survival_function(x_row)
  r0 <- as.numeric(expl$predict_function(x_row))
  # Extract the time points for survival curves
  times <- expl$times %||% seq_len(ncol(S0))
  # Create an empty list out, where each element corresponds to one scenario
  out <- vector("list", length(change_list)); names(out) <- names(change_list)

  # For each scenario, modify the specified features and apply biological constraints
  for (nm in names(change_list)) {
    x1 <- cf_perturb_multi(x_row, change_list[[nm]], constraints)
    S1 <- expl$predict_survival_function(x1)
    r1 <- as.numeric(expl$predict_function(x1))
    out[[nm]] <- list(times = times,
                      S_orig = as.numeric(S0[1, ]), S_cf = as.numeric(S1[1, ]),
                      risk_orig = r0, risk_cf = r1, x_cf = x1)
  }
  out
}

#' Assess stability of counterfactual predictions
#'
#' Evaluates how sensitive model risk predictions are to small random perturbations.
#' Jitter around a base change and report risk mean/sd
#'
#' @param expl A Survex explainer object.
#' @param x_row A one-row data frame of covariates.
#' @param base_change A named numeric vector of base relative changes.
#' @param n Integer; number of random samples (default = 10).
#' @param jitter Numeric; maximum random deviation added to each change (default = 0.02).
#' @param seed Integer; random seed for reproducibility (default = 1).
#'
#' @return A named numeric vector with mean and standard deviation of
#'   predicted risks under jittered perturbations.
#'
#' @examples
#' # cf_stability(expl, patient[1, ], c(bmi = 0.05))
#' @export
cf_stability <- function(expl, x_row, base_change, n = 10, jitter = 0.02, seed = 1) {
  x_row <- .cols_like(expl, x_row)
  set.seed(seed)
  # Repeat n times: add small random noise to base_change and recompute risk
  risks <- replicate(n, {
    jit <- base_change + runif(length(base_change), -jitter, jitter)
    names(jit) <- names(base_change)
    x1 <- cf_perturb_multi(x_row, jit)
    as.numeric(expl$predict_function(x1))
  })
  # If sd is small, it indicates that the model is stable under small perturbations
  c(mean = mean(risks), sd = sd(risks))
}
