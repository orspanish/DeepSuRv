#' Create a survex explainer object for a trained DeepSuRv model
#'
#' @param model     A trained DeepSuRv model.
#' @param data      A data.frame containing covariates + survival columns.
#' @param time_col  Name of survival time variable.
#' @param event_col Name of event indicator (1 = event).
#'
#' @return A survex explainer object.
#' @import survival
#' @import survex
#' @export
make_deepsurv_explainer <- function(model, data, time_col, event_col) {

  # build Surv response
  y <- survival::Surv(
    time  = data[[time_col]],
    event = data[[event_col]]
  )

  # extract covariates
  covars <- data[, !(names(data) %in% c(time_col, event_col)), drop = FALSE]

  # compute model linear predictor and risk scores
  lp <- model$predict_risk(covars)       # log-risk
  hr <- exp(lp)                           # hazard ratio

  # sort by increasing time (Cox convention)
  ord <- order(data[[time_col]])

  time_sorted  <- data[[time_col]][ord]
  event_sorted <- data[[event_col]][ord]
  hr_sorted    <- hr[ord]

  # compute baseline hazard manually
  unique_times <- sort(unique(time_sorted[event_sorted == 1]))
  H0 <- numeric(length(unique_times))
  cum_hazard <- 0
  for (i in seq_along(unique_times)) {
    t_i <- unique_times[i]
    d_i <- sum(event_sorted[time_sorted == t_i])            # events at time t_i
    riskset <- sum(hr_sorted[time_sorted >= t_i])           # risk set sum exp(η)

    cum_hazard <- cum_hazard + d_i / riskset
    H0[i] <- cum_hazard
  }

  # survival prediction function
  predict_survival_fun <- function(model, newdata, times) {

    lp_new <- model$predict_risk(newdata)
    hr_new <- exp(lp_new)

    # interpolate H0 onto requested time grid
    H0_interp <- approx(
      x = unique_times,
      y = H0,
      xout = times,
      method = "linear",
      rule = 2
    )$y

    # compute S(t | x)
    S <- outer(hr_new, H0_interp, function(r, h) exp(-r * h))
    return(S)
  }

  # risk prediction function
  predict_fun <- function(model, newdata) {
    model$predict_risk(newdata)
  }

  # make survex explainer
  expl <- survex::explain_survival(
    model = model,
    data = covars,
    y = y,
    predict_function = predict_fun,
    predict_survival_function = predict_survival_fun,
    times = unique_times,
    type = "risk"
  )

  return(expl)
}

#' Prepare data for DeepSurv training
#'
#' This function preprocesses a dataset for use in DeepSurv. It removes
#' incomplete cases, scales numeric variables, converts character/factor
#' variables to numeric encodings, constructs training data torch tensors, and
#' orders observations by decreasing survival time.
#'
#' @param data A `data.frame` containing covariates and survival outcome variables.
#' @param time_col A string giving the name of the survival time column.
#' @param event_col A string giving the name of the event indicator column
#'   (typically 1 = event, 0 = censored).
#'
#' @return A list containing:
#' \describe{
#'   \item{X_mat}{A processed version of the covariate matrix with numeric scaling
#'                   and factor/character conversion applied.}
#'   \item{dat}{A matrix of the processed covariate data with the addition of the
#'               time and event data.}
#' }
#'
#' @examples
#' \dontrun{
#' train_obj <- prep_data(mydata, time_col = "time", event_col = "status")
#' }
#' @export
prep_data <- function(data, time_col, event_col) {
  # remove rows with any missing values
  complete_rows <- which(rowSums(is.na(data)) == 0)
  data <- data[complete_rows, ]

  # extract covariate matrix
  X <- data[, !(names(data) %in% c(time_col, event_col)), drop = FALSE]

  # identify variable types
  num_tag <- sapply(X, is.numeric)
  cha_tag <- sapply(X, is.character)
  fac_tag <- sapply(X, is.factor)

  # numeric columns
  if (any(num_tag)) {
    X[, num_tag] <- unname(scale(X[, num_tag, drop = FALSE]))
  }

  # character columns (convert to factor, then numeric)
  if (any(cha_tag)) {
    X[, cha_tag] <- lapply(X[, cha_tag, drop = FALSE], function(col) {
      as.numeric(as.factor(col))
    })
  }

  # factor columns
  if (any(fac_tag)) {
    X[, fac_tag] <- lapply(X[, fac_tag, drop = FALSE], as.numeric)
  }

  # ensure X is a numeric matrix
  X_mat <- as.matrix(X)
  full_dat <- cbind(X_mat, data[, c(time_col, event_col)])

  return(list(X_mat = X_mat, dat = full_dat))
}
