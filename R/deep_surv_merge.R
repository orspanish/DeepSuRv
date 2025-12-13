#' DeepSuRv Neural Network Model (R6 Class)
#'
#' An R6-based DeepSurv neural network for survival analysis using `torch`.
#' Supports feature standardization, training, risk prediction, and bootstrap C-index evaluation.
#'
#' @name DeepSuRv
#' @docType class
#' @export
#'
#' @field model The underlying `nn_module` representing the neural network.
#' @field n_in Number of input features.
#' @field hidden_layers Integer vector specifying hidden layer sizes.
#' @field dropout Dropout probability applied to hidden layers.
#' @field learning_rate Learning rate for Adam optimizer.
#' @field n_epochs Number of epochs used during training.
#' @field mu Column means used for feature standardization.
#' @field sigma Column standard deviations used for feature standardization.
#'
#' @return A fully constructed DeepSuRv R6 object
#'
#' @import torch
#' @import survival
DeepSuRv <- R6::R6Class(
  classname = "DeepSuRv",
  public = list(
    model = NULL,
    n_in = NULL,
    hidden_layers = NULL,
    dropout = NULL,
    learning_rate = NULL,
    n_epochs = NULL,
    mu = NULL,
    sigma = NULL,

    #' @description Initialize a DeepSuRv object
    #' @param n_in Number of input features.
    #' @param hidden_layers Integer vector specifying hidden layer sizes.
    #' @param dropout Dropout probability.
    #' @param learning_rate Learning rate for Adam optimizer.
    #' @param n_epochs Number of epochs.
    #' @return A DeepSuRv object
    initialize = function(n_in, hidden_layers = c(), dropout = 0.0, learning_rate = 0.01, n_epochs = 50) {
      self$n_in <- n_in
      self$hidden_layers <- hidden_layers
      self$dropout <- dropout
      self$learning_rate <- learning_rate
      self$n_epochs <- n_epochs

      self$model <- nn_module(
        "DeepSurv",
        initialize = function(n_in, hidden, dropout) {
          self$layers <- nn_module_list()
          in_dim <- n_in
          for (h in hidden) {
            self$layers$append(nn_linear(in_dim, h))
            self$layers$append(nn_relu())
            if (dropout > 0) self$layers$append(nn_dropout(p = dropout))
            in_dim <- h
          }
          self$out <- nn_linear(in_dim, 1)
        },
        forward = function(x) {
          for (i in seq_len(length(self$layers))) {
            x <- self$layers[[i]](x)
          }
          self$out(x)
        }
      )(n_in = n_in, hidden = hidden_layers, dropout = dropout)
    },

    #' @description Compute and store column means and standard deviations for features.
    #' @param X Feature matrix (numeric matrix or data.frame).
    set_standardization = function(X) {
      self$mu <- colMeans(X)
      self$sigma <- apply(X, 2, sd)
      self$sigma[self$sigma == 0] <- 1
    },

    #' @description Standardize a feature matrix using stored mean and SD.
    #' @param X Feature matrix to standardize.
    #' @return Standardized feature matrix.
    standardize = function(X) {
      scale(X, center = self$mu, scale = self$sigma)
    },

    #' @description Compute negative log partial likelihood (Cox model loss).
    #' @param pred Tensor of predicted risk scores.
    #' @param t Tensor of event/censoring times.
    #' @param e Tensor of event indicators (1 = event, 0 = censored).
    #' @return Negative log partial likelihood (torch tensor).
    nll_loss = function(pred, t, e) {
      risk <- pred
      hazard_ratio <- torch_exp(risk)
      ord <- order(-as_array(t))
      hazard_ratio_sorted <- hazard_ratio[ord]
      log_risk <- torch_log(torch_cumsum(hazard_ratio_sorted, dim = 1))
      neg_ll <- -torch_sum((risk[ord] - log_risk) * e[ord])
      neg_ll / t$size(1)
    },

    #' @description Train the DeepSuRv model.
    #' @param X_train Training feature matrix.
    #' @param t_train Event/censoring times.
    #' @param e_train Event indicator vector.
    #' @param verbose Logical; print loss updates every 10 epochs.
    train = function(X_train, t_train, e_train, verbose = TRUE) {
      X_train_std <- self$standardize(X_train)
      X_tensor <- torch_tensor(X_train_std, dtype = torch_float())
      t_tensor <- torch_tensor(as.numeric(t_train), dtype = torch_float())
      e_tensor <- torch_tensor(as.numeric(e_train), dtype = torch_float())

      optimizer <- optim_adam(self$model$parameters, lr = self$learning_rate)

      for (epoch in 1:self$n_epochs) {
        self$model$train()
        optimizer$zero_grad()
        pred <- self$model(X_tensor)
        loss <- self$nll_loss(pred, t_tensor, e_tensor)
        loss$backward()
        optimizer$step()
        if (verbose && epoch %% 10 == 0) cat("Epoch:", epoch, "Loss:", as.numeric(loss$item()), "\n")
      }
    },

    #' @description Predict risk scores for new data.
    #' @param X Feature matrix.
    #' @return Numeric vector of predicted risk scores.
    predict_risk = function(X) {
      X_std <- self$standardize(X)
      X_tensor <- torch_tensor(X_std, dtype = torch_float())
      self$model$eval()
      with_no_grad({
        as.numeric(self$model(X_tensor)$squeeze())
      })
    },

    #' @description Compute bootstrap C-index confidence intervals.
    #' @param X Feature matrix.
    #' @param t Event/censoring times.
    #' @param e Event indicator vector.
    #' @param n_boot Number of bootstrap iterations (default 500).
    #' @param seed Random seed (default 42).
    #' @return List with `cindex`, `lower`, `upper`.
    bootstrap_cindex = function(X, t, e, n_boot = 500, seed = 42) {
      set.seed(seed)
      n <- length(t)
      scores <- self$predict_risk(X)
      c_hat <- survConcordance(Surv(t, e) ~ scores)$concordance
      boot_stats <- numeric(n_boot)
      for (i in 1:n_boot) {
        idx <- sample(1:n, n, replace = TRUE)
        boot_stats[i] <- survConcordance(Surv(t[idx], e[idx]) ~ scores[idx])$concordance
      }
      lower <- quantile(boot_stats, 0.025, na.rm = TRUE)
      upper <- quantile(boot_stats, 0.975, na.rm = TRUE)
      list(cindex = c_hat, lower = lower, upper = upper)
    }
  )
)
