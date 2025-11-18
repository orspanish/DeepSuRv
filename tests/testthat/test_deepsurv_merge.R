library(testthat)
library(torch)
library(jsonlite)
library(DeepSuRv)

test_that("DeepSuRv initialization works", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4, 2))
  expect_s3_class(model, "nn_module")
  expect_true(length(model$model) > 0) # Confirm layers exist
})

test_that("Forward pass produces correct output shape", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4, 2))
  x <- torch_randn(4, 3)  # 4 samples, 3 features
  out <- model$forward(x)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(dim(out), c(4,1))

  model <- DeepSuRv(n_in = 3, hidden_layers = c(5))
  x <- torch_tensor(matrix(runif(9), nrow = 3, ncol = 3))

  out <- model$forward(x)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(dim(out), c(3, 1))
})

test_that("risk function returns forward output", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(5))
  x <- torch_tensor(matrix(runif(6), nrow = 2, ncol = 3))

  risk_out <- model$risk(x)
  expect_true(inherits(risk_out, "torch_tensor"))
  expect_equal(dim(risk_out), c(2,1))
})

test_that("predict_risk returns numeric vector", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4,2))
  x <- matrix(rnorm(4*3), 4, 3)
  preds <- model$predict_risk(x)
  expect_type(preds, "double")
  expect_length(preds, 4)

  # Confirm torch_tensor works
  x_tensor <- torch_tensor(x)
  preds2 <- model$predict_risk(x_tensor)
  expect_equal(preds, preds2)
})

test_that("predict_risk is deterministic", {
  model <- DeepSuRv(n_in = 2, hidden_layers = c(4), dropout = 0.5)
  x <- matrix(runif(4), nrow = 2)

  pred1 <- model$predict_risk(x)
  pred2 <- model$predict_risk(x)

  expect_equal(pred1, pred2)
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

test_that("get_loss_updates returns loss and update_step", {
  model <- DeepSuRv(n_in = 3, hidden_layers = 2)
  x <- torch_randn(5, 3)
  time <- torch_randn(5)
  event <- torch_ones(5)

  loss_info <- model$get_loss_updates(x, time, event, opt_fn = "sgd")

  expect_true(inherits(loss_info$loss, "torch_tensor"))
  expect_true(is.function(loss_info$update_step))
  expect_true(inherits(loss_info$optimizer, "torch_optimizer"))
})

test_that("prepare_data + predict_risk works end-to-end", {
  dataset <- list(
    x = matrix(runif(20), ncol = 2),
    e = sample(c(0,1), 10, replace = TRUE),
    t = runif(10, 1, 10)
  )
  prepped <- prepare_data(dataset)
  model <- DeepSuRv(n_in = 2, hidden_layers = c(3))
  preds <- model$predict_risk(prepped$x)
  expect_equal(length(preds), nrow(prepped$x))
})

test_that("train_model runs without error", {
  model <- DeepSuRv(n_in = 5, hidden_layers = c(4, 2))

  train_data <- list(
    x = torch_randn(10, 5),
    time = torch_tensor(runif(10), dtype = torch_float()),
    event = torch_tensor(sample(0:1, 10, replace = TRUE), dtype = torch_float())
  )

  expect_silent(model$train_model(train_data, n_epochs = 2, verbose = FALSE))
})

test_that("Concordance index handles small datasets", {
  model <- DeepSuRv(n_in = 3, hidden_layers = c(4,2))
  x <- matrix(rnorm(4*3), 4, 3)
  time <- c(1,2,3,4)
  event <- c(1,0,1,1)
  ci <- model$get_concordance_index(x, time, event)
  expect_true(is.numeric(ci) || is.na(ci))
  expect_gte(ci, 0)
  expect_lte(ci, 1)
})
