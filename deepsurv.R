prepare_data <- function(dataset, standardize = TRUE, offset = NULL, scale = NULL) {
  x <- dataset$x
  e <- dataset$e
  t <- dataset$t
  # Optional - standardize
  if (standardize) {
    std <- standardize_x(x, offset, scale)
    x <- std$x; offset <- std$offset; scale <- std$scale
  }
  # Sort (descending)
  sort_idx <- order(t, decreasing = TRUE)
  x <- x[sort_idx, , drop = FALSE]
  e <- e[sort_idx]
  t <- t[sort_idx]
  # Return standardized + sorted data
  return(list(x = x, e = e, t = t, offset = offset, scale = scale))
}