test_that("GBSG2 dataset loads", {
  # Ensure the correct conda env is activated
  reticulate::use_condaenv("survdata_env", required = FALSE)
  reticulate::py_config() # Force Python initialization

  skip_if_no_python <- function() {
    if (!reticulate::py_available(initialize = TRUE))
      skip("Python not available")

    if (!reticulate::py_module_available("survdata.datasets"))
      skip("survdata Python module not installed")
  }

  skip_if_no_python()

  Xy <- load_survdata_dataset("gbsg2")
  expect_equal(length(Xy), 2)
})

