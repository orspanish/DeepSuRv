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
#' @import jsonlite
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
    self$eval()

    with_no_grad({
      out <- self$risk(x)
    })

    as.numeric(out$squeeze())
  },

  # -----------------------------------------------------------------------
  # Negative log partial likelihood (Cox)
  negative_log_likelihood = function(x, time, event) {
    pred <- self$forward(x)$squeeze()
    order <- order(as.numeric(time), decreasing = TRUE)
    pred <- pred[order]
    event <- event[order]
    risk <- torch_exp(pred)
    denom <- torch_cumsum(risk, dim = 1)
    log_cumhaz <- torch_log(denom)
    loss <- -torch_mean((pred - log_cumhaz) * event)
    loss
  },

  # -----------------------------------------------------------------------
  # Loss + updates
  get_loss_updates = function(x, time, event,
                              L1_reg = 0.0, L2_reg = 0.001,
                              max_norm = NULL, momentum = 0.9,
                              optimizer_fn = optim_sgd,
                              learning_rate = NULL) {

    loss <- self$negative_log_likelihood(x, time, event)

    # Regularization
    if (L1_reg > 0 || L2_reg > 0) {
      l1_penalty <- torch_tensor(0, dtype = torch_float())
      l2_penalty <- torch_tensor(0, dtype = torch_float())

      for (p in self$parameters()) {
        if (L1_reg > 0) l1_penalty <- l1_penalty + torch_sum(torch_abs(p))
        if (L2_reg > 0) l2_penalty <- l2_penalty + torch_sum(p$pow(2))
      }

      loss <- loss + L1_reg * l1_penalty + L2_reg * l2_penalty
    }

    # Optimizer
    lr_val <- ifelse(is.null(learning_rate), self$learning_rate, learning_rate)
    optimizer <- optimizer_fn(self$parameters(),
                              lr = lr_val,
                              momentum = momentum,
                              nesterov = TRUE)

    update_step <- function() {
      optimizer$zero_grad()
      loss$backward()
      if (!is.null(max_norm))
        nn_utils_clip_grad_norm_(self$parameters(), max_norm)
      optimizer$step()
    }

    self$update_step <- update_step
    list(loss = loss, optimizer = optimizer)
  },

  # -----------------------------------------------------------------------
  # Train + valid functions (wrappers for theano-style callable)
  get_train_valid_fn = function(L1_reg, L2_reg, learning_rate, ...) {

    # Training function: compute loss + apply updates
    train_fn <- function(x, time, event) {
      loss_updates <- self$get_loss_updates(
        x = x, time = time, event = event,
        L1_reg = L1_reg, L2_reg = L2_reg,
        learning_rate = learning_rate,
        deterministic = FALSE, ...
      )

      self$update_step()
      return(loss_updates$loss)
    }

    # Validation function: deterministic loss, no updates
    valid_fn <- function(x, time, event) {
      self$eval()
      with_no_grad({
        loss_val <- self$negative_log_likelihood(x, time, event)
      })
      self$train()
      return(loss_val)
    }

    list(train_fn = train_fn, valid_fn = valid_fn)
  },

  # -----------------------------------------------------------------------
  train_model = function(train_data, n_epochs = 50,
                         valid_data = NULL, early_stopping = TRUE,
                         patience = 10, verbose = TRUE) {

    x <- train_data$x
    time <- train_data$time
    event <- train_data$event

    opt_info <- self$get_loss_updates(x, time, event)
    best_loss <- Inf
    no_improve <- 0

    for (epoch in 1:n_epochs) {
      self$update_step()
      loss_val <- as.numeric(opt_info$loss$item())

      if (verbose)
        cat(sprintf("Epoch %d/%d - Loss: %.4f\n", epoch, n_epochs, loss_val))

      if (loss_val < best_loss) {
        best_loss <- loss_val
        no_improve <- 0
      } else {
        no_improve <- no_improve + 1
      }

      if (early_stopping && no_improve >= patience) {
        cat("Early stopping triggered.\n")
        break
      }
    }
    invisible(TRUE)
  },

  # -----------------------------------------------------------------------
  save_model = function(filename, weights_file = NULL) {
    info <- list(
      learning_rate = self$learning_rate,
      standardize = self$standardize
    )
    writeLines(jsonlite::toJSON(info, auto_unbox = TRUE), con = filename, useBytes = TRUE)
    if (!is.null(weights_file)) self$save_weights(weights_file)
    invisible(TRUE)
  },

  save_weights = function(filename) {
    torch_save_state_dict(self$state_dict(), filename)
    cat("Saved weights to", filename, "\n")
  },

  load_weights = function(filename) {
    state <- torch_load(filename)
    self$load_state_dict(state)
    cat("Loaded weights from", filename, "\n")
  },

  to_json = function() {
    jsonlite::toJSON(list(learning_rate = self$learning_rate), auto_unbox = TRUE)
  },

  get_concordance_index = function(x, time, event) {
    preds <- self$predict_risk(x)
    order <- order(time)
    concordant <- 0
    comparable <- 0
    for (i in seq_along(time)) {
      for (j in seq_along(time)) {
        if (time[i] < time[j] && event[i] == 1) {
          comparable <- comparable + 1
          if (preds[i] > preds[j]) concordant <- concordant + 1
        }
      }
    }
    ci <- concordant / comparable
    ci
  }
)
