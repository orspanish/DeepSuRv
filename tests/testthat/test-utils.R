# Mock setup for testing utility functions
#
# The "lasagne" object is normally defined in the higher-level model initialization code (outside this utilities module).
# Since this test file runs in isolation, we manually create a mock "lasagne" structure to simulate its presence.
#
# The use of "<<-" ensures that "lasagne" is assigned globally,
# so that the DeepSurv functions under test can access it within the package namespace during test execution.
lasagne <<- list(
  updates = list(
    sgd = "lasagne.updates.sgd",
    adam = "lasagne.updates.adam",
    rmsprop = "lasagne.updates.rmsprop"
  )
)

# test 1：get_optimizer_from_str
test_that("get_optimizer_from_str returns correct optimizer", {
  expect_equal(get_optimizer_from_str("sgd"), "lasagne.updates.sgd")
  expect_equal(get_optimizer_from_str("adam"), "lasagne.updates.adam")
  expect_equal(get_optimizer_from_str("rmsprop"), "lasagne.updates.rmsprop")
  expect_null(get_optimizer_from_str("none"))
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
