.onLoad <- function(libname, pkgname) {
  # Try to activate the environment — no installation, just activation
  try({
    reticulate::use_condaenv("survdata_env", required = FALSE)
  }, silent = TRUE)
}
