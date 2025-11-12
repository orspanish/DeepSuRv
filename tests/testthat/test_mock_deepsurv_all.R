library(testthat)
library(R6)
library(jsonlite)
library(survival)

context("Testing all MockDeepSurv functions")

# Load the mock train file


# ----------------------------
# Mock Data
# ----------------------------
mock_data <- list(
  x = matrix(rnorm(10), nrow = 5, ncol = 2),
  t = 1:5,
  e = c(1,0,1,0,1)
)

# ----------------------------
# Create model
# ----------------------------
model <- MockDeepSurv$new(n_params = 3)

# ----------------------------
# Tests
# ----------------------------
test_that("risk returns correct length", {
  expect_length(model$risk(n = nrow(mock_data$x)), nrow(mock_data$x))
  expect_length(model$risk(deterministic = TRUE, n = nrow(mock_data$x)), nrow(mock_data$x))
})

test_that("get_concordance_index returns numeric between 0 and 1", {
  ci <- model$get_concordance_index(mock_data$x, mock_data$t, mock_data$e)
  expect_type(ci, "double")
  expect_gte(ci, 0)
  expect_lte(ci, 1)
})

test_that("negative_log_likelihood returns numeric", {
  nll <- model$negative_log_likelihood(mock_data$e)
  expect_type(nll, "double")
})

test_that("get_loss_updates runs without error", {
  model$E <- mock_data$e
  res <- model$get_loss_updates()
  expect_true(all(c("loss","updates") %in% names(res)))
})

test_that("train function runs without error and returns expected history", {
  history <- train(model, train_data = mock_data, n_epochs = 2, verbose = FALSE)
  expect_true(all(c("best_validation_loss","best_params","best_params_idx") %in% names(history)))
})

test_that("save_weights and save_model run without error", {
  tmp_weights <- tempfile()
  tmp_file <- tempfile()
  expect_silent(model$save_weights(tmp_weights, verbose = FALSE))
  expect_silent(model$save_model(tmp_file, tmp_weights, verbose = FALSE))
})

test_that("to_json returns valid JSON", {
  json_out <- model$to_json()
  expect_true(jsonlite::validate(json_out))
})
