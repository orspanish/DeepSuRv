library(torch)
library(readr)
library(dplyr)

# set seed
set.seed(42)

# ============================================================
# Hyperparameters (reproduced from original paper)
# ============================================================
hp_gbsg <- list(
  nodes_per_layer = 8,
  n_layers = 1,
  lr = 0.154,
  dropout = 0.661,
  n_epochs = 50
)

hp_whas <- list(
  nodes_per_layer = 48,
  n_layers = 2,
  lr = 0.067,
  dropout = 0.147,
  n_epochs = 50
)

# ============================================================
# Helper: Build hidden layer vector
# ============================================================
make_hidden <- function(hp) {
  rep(hp$nodes_per_layer, hp$n_layers)
}

# ============================================================
# ===============  WHAS Dataset ==============================
# ============================================================

whas_train_file <- "inst/extdata/train_test_data/whas_train.csv"
whas_test_file  <- "inst/extdata/train_test_data/whas_test.csv"

whas_train <- read_csv(whas_train_file, show_col_types = FALSE)
whas_test  <- read_csv(whas_test_file, show_col_types = FALSE)

x_train <- as.matrix(select(whas_train, starts_with("x")))
t_train <- whas_train$time
e_train <- whas_train$event

x_test  <- as.matrix(select(whas_test, starts_with("x")))
t_test  <- whas_test$time
e_test  <- whas_test$event

model_whas <- DeepSuRv$new(
  n_in = ncol(x_train),
  hidden_layers = make_hidden(hp_whas),
  dropout = hp_whas$dropout,
  learning_rate = hp_whas$lr,
  n_epochs = hp_whas$n_epochs
)

model_whas$set_standardization(x_train)
model_whas$train(x_train, t_train, e_train, verbose = TRUE)

whas_res <- model_whas$bootstrap_cindex(
  x_test, t_test, e_test, n_boot = 500
)

cat(sprintf(
  "\nWHAS DeepSurv C-index: %.4f | 95%% CI: %.4f - %.4f\n",
  whas_res$cindex, whas_res$lower, whas_res$upper
))


# ============================================================
# ===============  GBSG Dataset ==============================
# ============================================================

gbsg_train_file <- "inst/extdata/train_test_data/gbsg_train.csv"
gbsg_test_file  <- "inst/extdata/train_test_data/gbsg_test.csv"

gbsg_train <- read_csv(gbsg_train_file, show_col_types = FALSE)
gbsg_test  <- read_csv(gbsg_test_file, show_col_types = FALSE)

x_train <- as.matrix(select(gbsg_train, starts_with("x")))
t_train <- gbsg_train$time
e_train <- gbsg_train$event

x_test  <- as.matrix(select(gbsg_test, starts_with("x")))
t_test  <- gbsg_test$time
e_test  <- gbsg_test$event

model_gbsg <- DeepSuRv$new(
  n_in = ncol(x_train),
  hidden_layers = make_hidden(hp_gbsg),
  dropout = hp_gbsg$dropout,
  learning_rate = hp_gbsg$lr,
  n_epochs = hp_gbsg$n_epochs
)

model_gbsg$set_standardization(x_train)
model_gbsg$train(x_train, t_train, e_train, verbose = TRUE)

gbsg_res <- model_gbsg$bootstrap_cindex(
  x_test, t_test, e_test, n_boot = 500
)

cat(sprintf(
  "\nGBSG DeepSurv C-index: %.4f | 95%% CI: %.4f - %.4f\n",
  gbsg_res$cindex, gbsg_res$lower, gbsg_res$upper
))


# ============================================================
# Summary
# ============================================================

cat(sprintf(
  "WHAS DeepSurv: %.4f (%.4f–%.4f)\n",
  whas_res$cindex, whas_res$lower, whas_res$upper
))

cat(sprintf(
  "GBSG DeepSurv: %.4f (%.4f–%.4f)\n",
  gbsg_res$cindex, gbsg_res$lower, gbsg_res$upper
))

