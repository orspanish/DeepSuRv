library(testthat)
library(torch)
library(jsonlite)
library(DeepSuRv)  # your package

test_that("DeepSuRv initialization works", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4, 2))
  expect_s3_class(model, "nn_module")
})

test_that("Forward pass produces correct output shape", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4, 2))
  x <- torch_randn(4, 3)  # 4 samples, 3 features
  out <- model$forward(x)
  expect_equal(dim(out), c(4,1))
})

test_that("Predict risk returns numeric vector", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4,2))
  x <- matrix(rnorm(4*3), 4, 3)
  preds <- model$predict_risk(x)
  expect_type(preds, "double")
  expect_length(preds, 4)
})

test_that("Negative log-likelihood computes without error", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4,2))
  data <- list(
    x = torch_randn(4,3),
    time = c(1,2,3,4),
    event = c(1,0,1,1)
  )
  loss <- model$negative_log_likelihood(data$x, data$time, data$event)
  expect_s3_class(loss, "torch_tensor")
})

test_that("Concordance index handles small datasets", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4,2))
  x <- matrix(rnorm(4*3), 4, 3)
  time <- c(1,2,3,4)
  event <- c(1,0,1,1)
  ci <- model$get_concordance_index(x, time, event)
  expect_true(is.numeric(ci) || is.na(ci))
})
