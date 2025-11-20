test_that("make_deepsurv_explainer creates a valid survex explainer", {

  # create mock DeepSuRv model
  mock_model <- list()

  # simple deterministic prediction function
  mock_model$predict_risk <- function(newdata) {
    # return a numeric vector of length nrow(newdata)
    rowSums(as.matrix(newdata)) * 0.1
  }

  class(mock_model) <- "DeepSuRv"

  # build small synthetic dataset
  set.seed(1)
  df <- data.frame(
    x1 = rnorm(5),
    x2 = rnorm(5),
    time = rexp(5, rate = 0.2),
    event = sample(c(0,1), 5, replace = TRUE)
  )

  # call function
  expl <- make_deepsurv_explainer(
    model = mock_model,
    data = df,
    time_col = "time",
    event_col = "event"
  )

  # tests

  # object is not null
  expect_true(!is.null(expl))

  # explainer should be a list (survex uses lists internally)
  expect_true(is.list(expl))

  # risk predictions should run
  preds <- expl$predict_function(df[, c("x1","x2")])
  expect_true(is.numeric(preds))
  expect_length(preds, 5)

  # ensure Surv object created correctly
  expect_true("y" %in% names(expl))
  expect_s3_class(expl$y, "Surv")

  # ensure covariates stored correctly
  expect_true("data" %in% names(expl))
  expect_identical(colnames(expl$data), c("x1", "x2"))
})
