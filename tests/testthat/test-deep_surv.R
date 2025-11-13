test_that("Maria's functions work with Olivia's", {
  # --- 1. Create Toy Dataset and Save as HDF5 ---
  cat("Creating toy HDF5 dataset...\n")

  h5file <- H5File$new("toy_data.h5", mode = "w")

  for (grp in c("train", "valid", "test")) {
    g <- h5file$create_group(grp)
    g[["x"]] <- matrix(rnorm(100), ncol = 5)   # 20 subjects × 5 features
    g[["t"]] <- runif(20, 1, 10)               # survival times
    g[["e"]] <- rbinom(20, 1, 0.7)             # event indicators
  }
  h5file$close_all()

  cat("✅ Toy HDF5 file created: toy_data.h5\n\n")

  # --- 2. Load Datasets ---
  datasets <- load_datasets("toy_data.h5")
  cat("✅ Loaded datasets: ", paste(names(datasets), collapse = ", "), "\n\n")

  # --- 3. Format to Data Frame (optional inspection) ---
  train_df <- format_dataset_to_df(datasets$train, duration_col = 1, event_col = 1)
  cat("✅ train_df preview:\n")
  print(head(train_df))

  # --- 4. Prepare Training Data ---
  train_prepared <- prepare_data(datasets$train, standardize = TRUE)
  cat("\n✅ Prepared training data structure:\n")
  print(str(train_prepared))

  # --- 5. Initialize DeepSuRv Model ---
  n_in <- ncol(train_prepared$x)
  model <- DeepSuRv(
    n_in = n_in,
    hidden_layers = c(32, 16),
    activation = "relu",
    dropout = 0.2,
    standardize = TRUE
  )

  # Assign the standardization parameters learned during preprocessing
  model$offset <- torch_tensor(train_prepared$offset)
  model$scale <- torch_tensor(train_prepared$scale)

  cat("\n✅ Model initialized with", n_in, "input features.\n")

  # --- 6. Forward Pass ---
  x_tensor <- torch_tensor(train_prepared$x, dtype = torch_float())
  log_risk <- model$forward(x_tensor)

  cat("\n✅ Forward pass successful! Log-risk output:\n")
  print(log_risk[1:5, ])

  # --- 7. Predict Risk (deterministic mode) ---
  preds <- model$predict_risk(train_prepared$x)
  cat("\n✅ Predicted log-risk values:\n")
  print(head(preds))

  # --- 8. Sanity Checks ---
  stopifnot(dim(log_risk)[1] == nrow(train_prepared$x))
  stopifnot(length(preds) == nrow(train_prepared$x))
  stopifnot(all(diff(train_prepared$t) <= 0))  # sorted descending

  cat("\n🎉 All integration tests passed! Your preprocessing functions and model work together correctly.\n")

})
