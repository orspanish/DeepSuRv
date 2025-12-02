# ---- Utility: safe Python → R conversion ----
convert_py <- function(obj) {
  r <- py_to_r(obj)

  # 1D numpy array
  if (is.numeric(r) && is.null(dim(r))) {
    return(data.frame(value = r))
  }

  # 2D numpy
  if (is.matrix(r)) {
    return(as.data.frame(r))
  }

  # Python dict/list
  if (is.list(r)) {
    return(as.data.frame(r))
  }

  stop(paste("Cannot convert Python object:", paste(class(r), collapse = ",")))
}

# ---- Train model in Python ----
deepsurv_train <- function(x_train, y_train, x_val, y_val, params) {
  ds <- import_from_path("deepsurv_train", path = ".")
  py_model <- ds$train_deepsurv(
    x_train, y_train,
    x_val, y_val,
    params
  )
  return(py_model)
}

# ---- Predict in Python ----
deepsurv_predict <- function(py_model, x_test) {
  pred_py <- py_model$predict(x_test)
  convert_py(pred_py)
}

# ---- Concordance Index ----
cindex <- function(pred, time, event) {
  survival::concordance(Surv(time, event) ~ pred)$concordance
}

# ---- Train + eval DeepSurv on one dataset ----
run_single_benchmark <- function(dataset, tune_iters, repetition) {
  df <- load_survdata_dataset(dataset)

  # Train/val/test split
  set.seed(1000 + repetition)
  idx <- sample(seq_len(nrow(df)))

  n <- nrow(df)
  train_id <- idx[1:floor(0.6*n)]
  val_id   <- idx[(floor(0.6*n)+1):floor(0.8*n)]
  test_id  <- idx[(floor(0.8*n)+1):n]

  x_train <- as.matrix(df[train_id, !(names(df) %in% c("time", "event"))])
  x_val   <- as.matrix(df[val_id, !(names(df) %in% c("time", "event"))])
  x_test  <- as.matrix(df[test_id, !(names(df) %in% c("time", "event"))])

  y_train <- df[train_id, c("time", "event")]
  y_val   <- df[val_id, c("time", "event")]
  y_test  <- df[test_id, c("time", "event")]

  # Hyperparameter tuning
  best_c <- -Inf
  best_params <- NULL

  for (i in seq_len(tune_iters)) {
    params <- list(
      lr = runif(1, 1e-4, 1e-2),
      l2 = runif(1, 1e-6, 1e-3),
      hidden = sample(1:3, 1),
      width = sample(20:100, 1)
    )

    py_model <- deepsurv_train(x_train, y_train, x_val, y_val, params)
    pred_val <- deepsurv_predict(py_model, x_val)

    cidx <- cindex(pred_val$value, y_val$time, y_val$event)

    if (cidx > best_c) {
      best_c <- cidx
      best_params <- params
    }
  }

  # Final training on train+val
  x_train_full <- rbind(x_train, x_val)
  y_train_full <- rbind(y_train, y_val)

  py_model <- deepsurv_train(x_train_full, y_train_full, x_test, y_test, best_params)

  pred_test <- deepsurv_predict(py_model, x_test)
  cidx_test <- cindex(pred_test$value, y_test$time, y_test$event)

  list(
    dataset = dataset,
    repetition = repetition,
    cindex = cidx_test,
    params = best_params
  )
}

#' Run a single dataset experiment repeated times
#'
#' @param df Data frame with columns time, status, and features
#' @param repetitions Number of repeated train/test splits
#' @param tune_iters Number of hyperparameter search iterations for DeepSuRv
#' @param verbose Logical, print progress messages
#' @return List of results per repetition
#' @export
run_experiment <- function(df, repetitions = 10, tune_iters = 20, verbose = FALSE) {
  out <- vector("list", repetitions)

  for (i in seq_len(repetitions)) {
    message("Experiment repetition ", i)
    split <- train_test_split(df, prop = 0.8)

    # Cox model
    cox_res <- train_cox(split$train, split$test)
    # fix: pass correct train_df/time/event to concordance
    cox_res$cindex <- get_concordance_index(preds = cox_res$lp,
                                            time = split$test$time,
                                            event = split$test$status)

    # RSF model
    rsf_res <- train_rsf(split$train, split$test)
    rsf_res$cindex <- get_concordance_index(preds = rsf_res$lp,
                                            time = split$test$time,
                                            event = split$test$status)

    # DeepSuRv: hyperparameter search on train only
    search <- random_search_deepsurv(split$train, n_iter = tune_iters, n_epochs = 30, patience = 8, verbose = verbose)
    best <- search$best

    best_params <- list(
      lr = as.numeric(best$lr),
      L2 = as.numeric(best$L2),
      hidden = eval(best$hidden),
      dropout = as.numeric(best$dropout),
      batch_norm = as.logical(best$batch_norm)
    )

    ds_res <- train_deepsurv(split$train, split$test,
                             hyperparams = best_params,
                             n_epochs = 100, patience = 15,
                             verbose = verbose)
    ds_res$cindex <- get_concordance_index(preds = ds_res$lp,
                                           time = split$test$time,
                                           event = split$test$status)

    out[[i]] <- list(cox = cox_res,
                     rsf = rsf_res,
                     ds = ds_res,
                     hparams = best_params)
  }

  out
}

# ---- Main API ----
#' Run all benchmarks for selected datasets
#'
#' @param datasets Character vector of dataset names (as in survdata.datasets)
#' @param repetitions Number of repeated train/test splits
#' @param tune_iters Number of hyperparameter search iterations for DeepSuRv
#' @param save Logical, whether to save results to bench_results_dir()
#' @return List of results per dataset
#' @export
run_benchmarks <- function(datasets = c("whas500", "support"),
                           repetitions = 5,
                           tune_iters  = 10,
                           save        = TRUE) {

  ensure_survdata_available()
  reticulate::use_condaenv("survdata_env", required = TRUE)

  results <- list()

  for (dataset in datasets) {
    message("Running: ", dataset)

    # Load dataset from Python
    ds <- load_survdata_dataset(dataset)
    X <- as.data.frame(ds$X)
    y <- ds$y

    # Convert y to data.frame with time/status
    if (is.matrix(y) || is.data.frame(y)) {
      if (ncol(y) >= 2) {
        time <- as.numeric(y[, 1])
        status <- as.integer(y[, 2])
      } else {
        stop("Unexpected y format from survdata: expected 2 columns (time, event)")
      }
    } else {
      stop("Unexpected y format from survdata: not matrix or data.frame")
    }

    df <- cbind(time = time, status = status, X)
    colnames(df) <- make.names(colnames(df))

    # Run experiments
    res <- run_experiment(df, repetitions = repetitions, tune_iters = tune_iters)

    # Save results if requested
    if (save) {
      fname <- paste0(dataset, "_results.rds")
      safe_dir_save(res, fname)
    }

    results[[dataset]] <- res
  }

  results
}
