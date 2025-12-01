#' Setup Python Environment for survdata Access
#'
#' Creates a conda environment named "survdata_env" with the required Python
#' packages for accessing datasets from the `survival-datasets` library.
#'
#' Run this function once after installing the package.
#'
#' @export
setup_survdata_env <- function() {

  # Ensure reticulate initializes
  if (!reticulate::py_available(initialize = FALSE)) {
    message("reticulate not initialized yet; proceeding.")
  }

  # Check if Miniconda is installed
  if (!dir.exists(reticulate::miniconda_path())) {
    message("No Miniconda installation found. Installing Miniconda...")
    reticulate::install_miniconda()
  }

  # List existing conda environments
  envs <- reticulate::conda_list()$name

  # Create the survdata_env if missing
  if (!"survdata_env" %in% envs) {
    message("Creating conda environment 'survdata_env'...")
    reticulate::conda_create(
      envname = "survdata_env",
      packages = "python=3.10"
    )
  } else {
    message("Environment 'survdata_env' already exists.")
  }

  # Install required conda packages
  message("Installing Conda packages...")
  reticulate::conda_install(
    envname = "survdata_env",
    packages = c("numpy>=1.22.4", "pandas>=1.4.3", "shap>=0.41", "pyarrow>=11.0"),
    pip = FALSE
  )

  # Pip install packages (use pip argument, not method)
  message("Installing pip packages (scikit-survival, survival-datasets)...")
  reticulate::conda_install(
    envname = "survdata_env",
    packages = c("scikit-survival>=0.17.2", "survival-datasets"),
    pip = TRUE
  )

  message("Python environment 'survdata_env' is ready!")
}

#' Load a dataset from the Python survdata package
#'
#' @param name Dataset name (e.g. "seer", "support", "metabric")
#'
#' @return A list with components X and y (converted from Python)
#' @export
load_survdata_dataset <- function(name) {
  # Ensure we use the correct Python environment
  reticulate::use_condaenv("survdata_env", required = TRUE)

  datasets <- reticulate::import("survdata.datasets", delay_load = TRUE)

  func_name <- paste0("load_", name, "_dataset")

  if (!func_name %in% reticulate::py_list_attributes(datasets)) {
    stop("Dataset not found in survdata.datasets: ", func_name)
  }

  # Call the Python function
  out <- datasets[[func_name]]()

  # Convert Python tuple to R list (X, y)
  list(
    X = reticulate::py_to_r(out[[1]]),
    y = reticulate::py_to_r(out[[2]])
  )
}

#' List available survdata datasets
#' @export
list_survdata_datasets <- function() {
  # Ensure we use the correct Python environment
  reticulate::use_condaenv("survdata_env", required = TRUE)

  datasets <- reticulate::import("survdata.datasets", delay_load = TRUE)

  fns <- reticulate::py_list_attributes(datasets)
  loaders <- grep("^load_.*_dataset$", fns, value = TRUE)

  # Strip 'load_' prefix and '_dataset' suffix
  gsub("^load_|_dataset$", "", loaders)
}
