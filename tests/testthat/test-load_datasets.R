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
