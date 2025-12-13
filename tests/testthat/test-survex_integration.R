test_that("make_deepsurv_explainer creates a valid survex explainer", {

  # create mock DeepSuRv model as a list
  mock_model <- list(
    predict_risk = function(newdata) {
      rowSums(as.matrix(newdata)) * 0.1
    }
  )

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
  expect_true(!is.null(expl))
  expect_true(is.list(expl))

  # now we call predict_function providing both arguments, as the original function expects
  preds <- expl$predict_function(mock_model, df[, c("x1","x2")])
  expect_true(is.numeric(preds))
  expect_length(preds, 5)

  expect_true("y" %in% names(expl))
  expect_s3_class(expl$y, "Surv")

  expect_true("data" %in% names(expl))
  expect_identical(colnames(expl$data), c("x1", "x2"))
})
