test_that("format_dataset_to_df combines columns correctly", {
  dataset <- list(
    x = data.frame(a = 1:3, b = 4:6),
    t = data.frame(time = c(10, 5, 8)),
    e = data.frame(event = c(1, 0, 1))
  )

  df <- format_dataset_to_df(dataset, duration_col = "time", event_col = "event")

  # Output should be a data frame
  expect_s3_class(df, "data.frame")

  # Check columns
  expect_true(all(c("a", "b", "dt", "censor") %in% colnames(df)))

  # Check correct column lengths
  expect_equal(nrow(df), nrow(dataset$x))
})

test_that("format_dataset_to_df renames treatment column if provided", {
  dataset <- list(
    x = data.frame(trt = c(0, 1, 0), age = c(25, 60, 45)),
    t = data.frame(time = c(10, 5, 8)),
    e = data.frame(event = c(1, 0, 1))
  )

  df <- format_dataset_to_df(dataset, duration_col = "time", event_col = "event", trt_idx = 1)
  expect_true("treat" %in% colnames(df))
})
