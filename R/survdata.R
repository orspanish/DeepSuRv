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

  # Pip install packages
  message("Installing pip packages (scikit-survival, survival-datasets)...")
  reticulate::py_install(
    packages = c("scikit-survival>=0.17.2", "survival-datasets"),
    envname = "survdata_env",
    method = "pip"
  )

  message("Python environment 'survdata_env' is ready!")
}

load_survdata_module <- function() {
  if (!reticulate::py_module_available("survdata.datasets")) {
    stop(
      "The Python package 'survival-datasets' is not installed.\n",
      "Please run setup_survdata_env() first."
    )
  }

  reticulate::use_condaenv("survdata_env", required = TRUE)
  reticulate::import("survdata.datasets")
}

#' Load a dataset from the Python survdata package
#'
#' @param name Dataset name (e.g. "seer", "support", "metabric")
#'
#' @return A list with components X and y (converted from Python)
#' @export
load_survdata_dataset <- function(name) {
  datasets <- reticulate::import("survdata.datasets", delay_load = TRUE)

  func_name <- paste0("load_", name, "_dataset")

  if (!func_name %in% reticulate::py_list_attributes(datasets)) {
    stop("Dataset not found in survdata.datasets: ", func_name)
  }

  # Call the Python function
  out <- datasets[[func_name]]()

  # out is a Python tuple → reticulate converts to length-2 list automatically
  return(out)
}

#' List available survdata datasets
#' @export
list_survdata_datasets <- function() {
  datasets <- reticulate::import("survdata.datasets", delay_load = TRUE)

  fns <- reticulate::py_list_attributes(datasets)
  loaders <- grep("^load_.*_dataset$", fns, value = TRUE)

  gsub("^load_|_dataset$", "", loaders)
}
