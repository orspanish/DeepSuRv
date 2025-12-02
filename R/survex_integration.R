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
