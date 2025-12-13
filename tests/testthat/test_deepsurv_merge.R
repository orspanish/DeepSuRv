library(testthat)
library(torch)
library(survival)


test_that("DeepSuRv class initializes correctly", {
  n_features <- 3
  hidden <- c(4, 2)
  ds <- DeepSuRv$new(n_in = n_features, hidden_layers = hidden)

  expect_s3_class(ds, "DeepSuRv")
  expect_equal(ds$n_in, n_features)
  expect_equal(ds$hidden_layers, hidden)
  expect_true(inherits(ds$model, "nn_module"))
})

test_that("set_standardization and standardize work correctly", {
  set.seed(1)
  X <- matrix(rnorm(12), ncol = 3)
  ds <- DeepSuRv$new(n_in = ncol(X))

  ds$set_standardization(X)
  X_std <- ds$standardize(X)

  expect_equal(dim(X_std), dim(X))
  # Means approx 0, SD approx 1
  expect_true(all(abs(colMeans(X_std)) < 1e-8))
  expect_true(all(abs(apply(X_std, 2, sd) - 1) < 1e-8))
})

test_that("predict_risk returns numeric vector of correct length", {
  set.seed(1)
  X <- matrix(rnorm(12), ncol = 3)
  ds <- DeepSuRv$new(n_in = ncol(X))
  ds$set_standardization(X)

  risk <- ds$predict_risk(X)
  expect_type(risk, "double")
  expect_equal(length(risk), nrow(X))
})

test_that("bootstrap_cindex returns correct structure", {
  set.seed(1)
  X <- matrix(rnorm(12), ncol = 3)
  t <- c(5, 2, 8, 3)
  e <- c(1, 0, 1, 0)

  ds <- DeepSuRv$new(n_in = ncol(X))
  ds$set_standardization(X)

  res <- ds$bootstrap_cindex(X, t, e, n_boot = 5)
  expect_true(all(c("cindex", "lower", "upper") %in% names(res)))
  expect_type(res$cindex, "double")
  expect_type(res$lower, "double")
  expect_type(res$upper, "double")
})

test_that("nll_loss runs without error and returns tensor", {
  set.seed(1)
  X <- matrix(rnorm(12), ncol = 3)
  t <- torch_tensor(c(5, 2, 8, 3), dtype = torch_float())
  e <- torch_tensor(c(1, 0, 1, 0), dtype = torch_float())

  ds <- DeepSuRv$new(n_in = ncol(X))
  ds$set_standardization(X)
  X_tensor <- torch_tensor(ds$standardize(X), dtype = torch_float())
  pred <- ds$model(X_tensor)

  loss <- ds$nll_loss(pred, t, e)
  expect_true(inherits(loss, "torch_tensor"))
})
