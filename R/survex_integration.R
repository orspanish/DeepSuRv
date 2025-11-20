#' Create a survex explainer object for a trained DeepSuRv model
#'
#' @param model A trained DeepSuRv model object.
#' @param data A data.frame containing covariates + survival columns.
#' @param time_col Name of the survival time column.
#' @param event_col Name of the event indicator column (1 = event).
#'
#' @return A survex explainer object created by survex::explain_survival()
#' @import survival
#' @import survex
#' @export
make_deepsurv_explainer <- function(model, data, time_col, event_col) {

  # build Surv object
  y <- survival::Surv(time = data[[time_col]],
                      event = data[[event_col]])

  # extract covariates
  covars <- data[, !(names(data) %in% c(time_col, event_col)), drop = FALSE]

  # compute model linear predictor (log-risk scores)
  lp <- model$predict_risk(covars)  # numeric vector

  # fit Cox model with offset to estimate baseline hazard
  cox_fit <- survival::coxph(
    formula = y ~ offset(lp),
    ties   = "breslow"
  )

  base_haz <- survival::basehaz(cox_fit, centered = FALSE)

  # extract cumulative baseline hazard and times
  H0   <- base_haz$hazard      # cumulative hazard values
  time <- base_haz$time        # corresponding time grid

  # survival prediction function for survex
  predict_survival_fun <- function(newdata) {

    # convert log-risk to hazard ratio
    hr <- exp(model$predict_risk(newdata))

    # construct survival matrix: obs x time points
    S <- outer(hr, H0, FUN = function(r, h) exp(-r * h))

    # ensure numeric and well-formed
    storage.mode(S) <- "double"
    return(S)
  }

  # risk predictions
  predict_fun = function(newdata) {
    model$predict_risk(newdata)
  }

  # create survex explainer
  expl <- survex::explain_survival(
    model = model,
    data = covars,
    y = y,
    predict_function = predict_fun,
    times = time,
    predict_survival_function = predict_survival_fun,
    type = "risk"
  )

  return(expl)
}
