library(R6)
library(jsonlite)
library(survival)
library(stats) # for sd()

# ----------------------------
# Logger for testing
# ----------------------------
DeepSurvLogger <- R6::R6Class(
  "DeepSurvLogger",
  public = list(
    history = list(),
    initialize = function(name = "DeepSurv") {},
    logValue = function(name, value, epoch) {
      self$history[[paste0(name, "_", epoch)]] <- value
    },
    logMessage = function(msg) { cat(msg, "\n") },
    print_progress_bar = function(epoch, n_epochs, loss, ci, verbose = TRUE) {
      if (verbose) cat(sprintf("Epoch %d/%d - Loss: %.4f - C-index: %.3f\n", epoch, n_epochs, loss, ci))
    },
    shutdown = function() { invisible(TRUE) }
  )
)

# ----------------------------
# Mock DeepSurv Class
# ----------------------------
MockDeepSurv <- R6::R6Class(
  "MockDeepSurv",
  public = list(
    network = NULL,
    params = NULL,
    updates = NULL,
    restored_update_params = NULL,
    E = numeric(0),
    offset = NULL,
    scale = NULL,
    learning_rate = 0.01,
    L1_reg = 0.0,
    L2_reg = 0.0,
    lr_decay = 0.0,
    momentum = 0.0,
    standardize = TRUE,
    hyperparams = list(),

    initialize = function(n_params = 3) {
      self$params <- lapply(1:n_params, function(i) rnorm(5))
      self$hyperparams <- list(n_params = n_params, learning_rate = self$learning_rate)
      self$E <- numeric(0)
    },

    risk = function(deterministic = FALSE, n = NULL) {
      n <- ifelse(!is.null(n), n,
                  ifelse(length(self$E) > 0, length(self$E),
                         stop("E not set or n not provided")))
      if (deterministic) {
        return(rep(0.5, n))
      } else {
        return(runif(n, 0, 1))
      }
    },

    prepare_data = function(dataset) {
      list(x = dataset$x, t = dataset$t, e = dataset$e)
    },

    get_concordance_index = function(x, t, e, ...) {
      partial_hazards <- -self$risk(deterministic = TRUE, n = length(t))
      surv_obj <- survival::Surv(time = t, event = e)
      concord <- survival::concordance(surv_obj ~ partial_hazards)
      return(concord$concordance)
    },

    to_json = function() {
      jsonlite::toJSON(self$hyperparams, auto_unbox = TRUE, pretty = TRUE)
    },

    negative_log_likelihood = function(E, deterministic = FALSE) {
      risk_vals <- self$risk(deterministic = deterministic, n = length(E))
      -mean(risk_vals * E)
    },

    get_loss_updates = function(L1_reg = 0.0, L2_reg = 0.001,
                                update_fn = function(loss, params, ...) {
                                  lapply(params, function(p) p + rnorm(length(p), 0, 0.01))
                                },
                                max_norm = NULL,
                                deterministic = FALSE,
                                momentum = 0.9, ...) {
      if (length(self$E) == 0) stop("E not set")
      loss <- sum(self$E)/length(self$E) + runif(1, 0, 0.01)
      updates <- update_fn(loss, self$params, ...)
      self$updates <- updates
      return(list(loss = loss, updates = updates))
    },

    get_train_valid_fn = function(L1_reg, L2_reg, learning_rate, ...) {
      train_fn <- function(x, e) {
        self$E <- e
        res <- self$get_loss_updates(L1_reg = L1_reg, L2_reg = L2_reg, deterministic = FALSE, ...)
        self$params <- res$updates
        res$loss
      }
      valid_fn <- function(x, e) {
        self$E <- e
        res <- self$get_loss_updates(L1_reg = L1_reg, L2_reg = L2_reg, deterministic = TRUE, ...)
        res$loss
      }
      list(train_fn = train_fn, valid_fn = valid_fn)
    },

    save_weights = function(filename, verbose = TRUE) {
      if (verbose) cat("Mock saving weights to", filename, "\n")
      invisible(TRUE)
    },

    save_model = function(filename, weights_file = NULL, verbose = TRUE) {
      writeLines(self$to_json(), con = filename)
      if (!is.null(weights_file)) self$save_weights(weights_file, verbose = verbose)
    }
  )
)

# ============================
# Training Function
# ============================
train <- function(self,
                  train_data,
                  valid_data = NULL,
                  n_epochs = 5,
                  validation_frequency = 2,
                  patience = 10,
                  improvement_threshold = 0.99999,
                  patience_increase = 2,
                  logger = NULL,
                  update_fn = NULL,
                  verbose = TRUE,
                  ...) {

  if (is.null(logger)) logger <- DeepSurvLogger$new()

  if (!is.null(self$standardize) && self$standardize) {
    self$offset <- base::colMeans(train_data$x)
    self$scale <- apply(train_data$x, 2, stats::sd)
  }

  train_list <- self$prepare_data(train_data)
  x_train <- train_list$x; e_train <- train_list$e; t_train <- train_list$t

  if (!is.null(valid_data)) {
    valid_list <- self$prepare_data(valid_data)
    x_valid <- valid_list$x; e_valid <- valid_list$e; t_valid <- valid_list$t
  }

  funcs <- self$get_train_valid_fn(L1_reg = self$L1_reg,
                                   L2_reg = self$L2_reg,
                                   learning_rate = self$learning_rate,
                                   ...)
  train_fn <- funcs$train_fn
  valid_fn <- funcs$valid_fn

  best_validation_loss <- Inf
  best_params <- NULL
  best_params_idx <- -1
  start <- Sys.time()

  for (epoch in 1:n_epochs) {
    loss <- train_fn(x_train, e_train)
    ci_train <- self$get_concordance_index(x_train, t_train, e_train)

    if (!is.null(valid_data) && (epoch %% validation_frequency == 0)) {
      validation_loss <- valid_fn(x_valid, e_valid)
      ci_valid <- self$get_concordance_index(x_valid, t_valid, e_valid)
      if (validation_loss < best_validation_loss) {
        if (validation_loss < best_validation_loss * improvement_threshold) {
          patience <- max(patience, epoch * patience_increase)
        }
        best_params <- lapply(self$params, function(p) p)
        best_params_idx <- epoch
        best_validation_loss <- validation_loss
      }
      if (verbose) logger$print_progress_bar(epoch, n_epochs, loss, ci_train, verbose = verbose)
    }

    if (epoch >= patience) break
  }

  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (verbose) logger$logMessage(sprintf("Finished Training in %.2fs", elapsed))

  # Ensure these fields always exist in history
  logger$history$best_validation_loss <- ifelse(is.null(valid_data), NA_real_, best_validation_loss)
  logger$history$best_params <- ifelse(is.null(best_params), list(), best_params)
  logger$history$best_params_idx <- ifelse(is.na(best_params_idx), NA_integer_, best_params_idx)

  logger$history
}
