test_that("prepare_data standardizes and sorts correctly", {
  dataset <- list(
    x = matrix(1:6, ncol = 2),
    e = c(1, 0, 1),
    t = c(5, 2, 8)
  )

  result <- prepare_data(dataset)

  # Check output structure
  expect_true(all(c("x", "e", "t", "offset", "scale") %in% names(result)))

  # Check sorting (descending t)
  expect_true(all(diff(result$t) <= 0))

  # Check that e and t were sorted consistently
  expect_identical(result$e[1], dataset$e[3])  # largest t = 8, e=1

  # Standardization check
  expect_true(abs(mean(result$x[,1])) < 1e-8)
})
