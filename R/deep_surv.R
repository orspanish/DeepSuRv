#' DeepSurv Neural Network Model
#'
#' Constructs a feed-forward neural network that outputs a log hazard score,
#' equivalent to the core architecture of the Python DeepSurv implementation.
#'
#' @param n_in Integer. Number of input features.
#' @param hidden_layers Integer vector. Sizes of hidden layers.
#' @param activation Character. Activation function: "relu" or "selu".
#' @param dropout Numeric or NULL. Dropout probability per hidden layer.
#' @param batch_norm Logical. Whether to include batch normalization layers.
#' @param standardize Logical. Whether to apply input standardization.
#'
#' @return An `nn_module` representing the DeepSurv model.
#' @import torch
#'
#' @examples
#' model <- DeepSuRv(n_in = 10, hidden_layers = c(32, 16), activation = "relu")
#' @export
DeepSuRv <- nn_module(
  classname = "DeepSurv",

  initialize = function(n_in,
                        hidden_layers = c(),
                        activation = "relu",
                        dropout = NULL,
                        batch_norm = FALSE,
                        standardize = FALSE) {

    # Activation function
    act <- switch(
      activation,
      "relu" = nn_relu(),
      "selu" = nn_selu(),
      stop("Unknown activation function. Use 'relu' or 'selu'.")
    )

    layers <- list()
    input_dim <- n_in

    # Standardization parameters (not trainable)
    self$standardize <- standardize
    if (standardize) {
      self$offset <- torch_zeros(n_in)
      self$scale <- torch_ones(n_in)
    }

    # Build hidden layers
    for (h in hidden_layers) {
      layers <- append(layers, nn_linear(input_dim, h))

      if (batch_norm)
        layers <- append(layers, nn_batch_norm1d(h))

      layers <- append(layers, act)

      if (!is.null(dropout) && dropout > 0)
        layers <- append(layers, nn_dropout(dropout))

      input_dim <- h
    }

    # Final linear output → log hazard score
    layers <- append(layers, nn_linear(input_dim, 1))

    self$model <- nn_sequential(!!!layers)
  },

  # Take input feature, apply all layers of model (linear layers and activation
  # functions)
  # Returns a single log-risk score for each subject to be used in prediction
  forward = function(x) {
    if (self$standardize)
      x <- (x - self$offset) / self$scale
    self$model(x)
  },

  # Compute log-risk scores (forward output)
  # Returns a torch tensor of shape (n, 1)
  risk = function(x) {
    self$forward(x)
  },

  # Deterministic prediction (no dropout, eval mode)
  # Returns a numeric vector of predicted log-risk for each observation.
  predict_risk = function(x) {

    # Convert x to torch tensor if needed
    if (!inherits(x, "torch_tensor")) {
      x <- torch_tensor(as.matrix(x), dtype = torch_float())
    }

    # Deterministic mode (no dropout/batchnorm updates)
    self$eval

    with_no_grad({
      out <- self$risk(x)
    })

    as.numeric(out$squeeze())
  }
)
