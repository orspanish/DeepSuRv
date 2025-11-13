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

test_that("standardize_x handles zero-variance columns", {
  x <- matrix(rep(5, 6), ncol = 2)
  result <- standardize_x(x)
  expect_true(all(result$x == 0))
})
