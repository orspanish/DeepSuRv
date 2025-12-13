# Mock setup for testing utility functions
#
# The "lasagne" object is normally defined in the higher-level model initialization code (outside this utilities module).
# Since this test file runs in isolation, we manually create a mock "lasagne" structure to simulate its presence.
#
# The use of "<<-" ensures that "lasagne" is assigned globally,
# so that the DeepSurv functions under test can access it within the package namespace during test execution.

# test 1：get_optimizer_from_str
test_that("get_optimizer_from_str returns correct optimizer functions", {
  expect_identical(get_optimizer_from_str("sgd"), torch::optim_sgd)
  expect_identical(get_optimizer_from_str("adam"), torch::optim_adam)
  expect_identical(get_optimizer_from_str("rmsprop"), torch::optim_rmsprop)
  expect_null(get_optimizer_from_str("unknown"))

  # simple toy model
  model <- nn_linear(2,1)

  opt_fn <- get_optimizer_from_str("adam")
  opt <- opt_fn(model$parameters, lr = 0.01)

  expect_s3_class(opt, "torch_optimizer")
})

# test 2：standardize_dataset
test_that("standardize_dataset standardizes data correctly", {
  ds <- list(x = c(2, 4, 6))
  offset <- 2
  scale <- 2
  result <- standardize_dataset(ds, offset, scale)
  expect_equal(result$x, c(0, 1, 2))
})

# test 3：negative_log_likelihood
test_that("negative_log_likelihood returns numeric value", {
  fake_self <- list(risk = function(deterministic) c(0.1, 0.2, 0.3))
  E <- c(1, 0, 1)
  result <- negative_log_likelihood(fake_self, E)
  expect_true(is.numeric(result))
})

# test 4：bootstrap_metric
test_that("bootstrap_metric returns mean and confidence interval", {
  dataset <- list(x = 1:5)
  metric_fxn <- function(x) mean(x)
  result <- bootstrap_metric(metric_fxn, dataset, N = 5)
  expect_true("mean" %in% names(result))
  expect_true("confidence_interval" %in% names(result))
})

# test 5：calculate_recs_and_antirecs
test_that("calculate_recs_and_antirecs splits data correctly", {
  dataset <- list(
    x = matrix(c(1, 0, 1, 0, 1, 0), ncol = 2),
    t = c(1, 2, 3),
    e = c(1, 0, 1)
  )
  rec_trt <- c(0, 1, 1)
  true_trt <- c(0, 1, 0)
  result <- calculate_recs_and_antirecs(rec_trt, true_trt, dataset, print_metrics = FALSE)
  expect_true("rec_t" %in% names(result))
  expect_true("antirec_t" %in% names(result))
})

#test 6: test correct standardization of x
test_that("standardize_x standardizes correctly", {
  set.seed(1)
  x <- matrix(rnorm(9), ncol = 3)
  result <- standardize_x(x)
  # Check that means are approximately 0
  expect_true(all(abs(colMeans(result$x)) < 1e-8))
  # Check that standard deviations are approximately 1
  expect_true(all(abs(apply(result$x, 2, sd) - 1) < 1e-8))
  # Check offset and scale lengths
  expect_equal(length(result$offset), ncol(x))
  expect_equal(length(result$scale), ncol(x))
})

#test 7: test zero-variance handling
test_that("standardize_x handles zero-variance columns", {
  x <- matrix(rep(5, 6), ncol = 2)
  result <- standardize_x(x)
  expect_true(all(result$x == 0))
})

#test 8: dataset is formatted correctly
test_that("format_dataset_to_df combines columns correctly", {
  dataset <- list(
    x = data.frame(a = 1:3, b = 4:6),
    t = data.frame(time = c(10, 5, 8)),
    e = data.frame(event = c(1, 0, 1))
  )
  df <- format_dataset_to_df(dataset, duration_col = "time", event_col = "event")
  # Output should be a df
  expect_s3_class(df, "data.frame")
  # Check columns
  expect_true(all(c("a", "b", "dt", "censor") %in% colnames(df)))
  # Check correct column lengths
  expect_equal(nrow(df), nrow(dataset$x))
})

#test 9: test renaming of treatment column
test_that("format_dataset_to_df renames treatment column if provided", {
  dataset <- list(
    x = data.frame(trt = c(0, 1, 0), age = c(25, 60, 45)),
    t = data.frame(time = c(10, 5, 8)),
    e = data.frame(event = c(1, 0, 1))
  )
  df <- format_dataset_to_df(dataset, duration_col = "time", event_col = "event", trt_idx = 1)
  expect_true("treat" %in% colnames(df))
})


#test 10: test handling of h5 datasets
test_that("load_datasets reads HDF5 groups and returns matrices", {
  skip_if_not_installed("hdf5r")
  library(hdf5r)

  tmp <- tempfile(fileext = ".h5")
  h5 <- H5File$new(tmp, mode = "w")

  # --- TRAIN group (all written explicitly as matrices) ---
  train_grp <- h5$create_group("train")
  mat_x_train <- matrix(1:6, nrow = 3, ncol = 2)
  train_grp$create_dataset("x", robj = mat_x_train, dims = dim(mat_x_train))
  mat_t_train <- matrix(c(5, 2, 8), nrow = 3, ncol = 1)
  train_grp$create_dataset("t", robj = mat_t_train, dims = dim(mat_t_train))
  mat_e_train <- matrix(c(1, 0, 1), nrow = 3, ncol = 1)
  train_grp$create_dataset("e", robj = mat_e_train, dims = dim(mat_e_train))

  # --- VALID group ---
  valid_grp <- h5$create_group("valid")
  mat_x_valid <- matrix(7:12, nrow = 3, ncol = 2)
  valid_grp$create_dataset("x", robj = mat_x_valid, dims = dim(mat_x_valid))
  mat_t_valid <- matrix(c(1, 3, 4), nrow = 3, ncol = 1)
  valid_grp$create_dataset("t", robj = mat_t_valid, dims = dim(mat_t_valid))
  mat_e_valid <- matrix(c(0, 1, 0), nrow = 3, ncol = 1)
  valid_grp$create_dataset("e", robj = mat_e_valid, dims = dim(mat_e_valid))

  # flush & close
  h5$close_all()

  # now call the function under test
  datasets <- load_datasets(tmp)

  # checks
  expect_true(all(c("train", "valid") %in% names(datasets)))
  for (grp in names(datasets)) {
    expect_true(all(c("x", "t", "e") %in% names(datasets[[grp]])))
    expect_true(is.matrix(datasets[[grp]]$x))
    expect_true(is.matrix(datasets[[grp]]$t))
    expect_true(is.matrix(datasets[[grp]]$e))
    expect_equal(ncol(datasets[[grp]]$t), 1)
    expect_equal(ncol(datasets[[grp]]$e), 1)
  }
})
