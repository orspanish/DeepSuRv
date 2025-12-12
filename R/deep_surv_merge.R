#' DeepSuRv Neural Network Model (R6 + nn_module)
#'
#' R6-style DeepSurv implementation for survival analysis using torch `nn_module`.
#' Includes training, prediction, and bootstrap C-index evaluation.
#'
#' @param n_in Integer. Number of input features.
#' @param hidden_layers Integer vector. Sizes of hidden layers.
#' @param dropout Numeric. Dropout probability for each hidden layer.
#' @param learning_rate Numeric. Learning rate for optimizer.
#' @param n_epochs Integer. Number of epochs for training.
#'
#' @return An R6 object representing a DeepSurv neural network.
#' @import torch survival
#' @export
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

    initialize = function(n_in, hidden_layers = c(), dropout = 0.0, learning_rate = 0.01, n_epochs = 50) {
      self$n_in <- n_in
      self$hidden_layers <- hidden_layers
      self$dropout <- dropout
      self$learning_rate <- learning_rate
      self$n_epochs <- n_epochs

      # nn_module network
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

    # Standardize inputs
    set_standardization = function(X) {
      self$mu <- colMeans(X)
      self$sigma <- apply(X, 2, sd)
      self$sigma[self$sigma == 0] <- 1
    },
    standardize = function(X) {
      scale(X, center = self$mu, scale = self$sigma)
    },

    # Negative log partial likelihood
    nll_loss = function(pred, t, e) {
      risk <- pred
      hazard_ratio <- torch_exp(risk)
      ord <- order(-as_array(t))
      hazard_ratio_sorted <- hazard_ratio[ord]
      log_risk <- torch_log(torch_cumsum(hazard_ratio_sorted, dim = 1))
      neg_ll <- -torch_sum((risk[ord] - log_risk) * e[ord])
      neg_ll / t$size(1)
    },

    # Training function
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

    # Prediction
    predict_risk = function(X) {
      X_std <- self$standardize(X)
      X_tensor <- torch_tensor(X_std, dtype = torch_float())
      self$model$eval()
      with_no_grad({
        as.numeric(self$model(X_tensor)$squeeze())
      })
    },

    # Bootstrap C-index
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
