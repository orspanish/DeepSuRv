library(torch)
library(readr)
library(dplyr)

# --------------------------
# Bootstrap C-index using existing get_concordance_index
# --------------------------
bootstrap_cindex <- function(model, x, time, event, n_boot = 500, seed = 42) {
  set.seed(seed)
  n <- length(time)
  c_hat <- model$get_concordance_index(x, time, event)
  boot_stats <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    idx <- sample(seq_len(n), n, replace = TRUE)
    x_b <- x[idx, , drop = FALSE]
    time_b <- time[idx]
    event_b <- event[idx]
    boot_stats[i] <- model$get_concordance_index(x_b, time_b, event_b)
  }
  ci_lower <- quantile(boot_stats, 0.025, names = FALSE)
  ci_upper <- quantile(boot_stats, 0.975, names = FALSE)
  list(c_index = c_hat, ci95 = c(ci_lower, ci_upper))
}

# --------------------------
# Load datasets
# --------------------------
train_file <- "inst/extdata/train_test_data/whas_train.csv"
test_file  <- "inst/extdata/train_test_data/whas_test.csv"

train_data <- read_csv(train_file)
test_data  <- read_csv(test_file)

x_train <- as.matrix(train_data %>% select(starts_with("x")))
t_train <- train_data$time
e_train <- train_data$event

x_test <- as.matrix(test_data %>% select(starts_with("x")))
t_test <- test_data$time
e_test <- test_data$event

# --------------------------
# Initialize DeepSuRv
# --------------------------
model <- DeepSuRv(
  n_in = ncol(x_train),
  hidden_layers_sizes = c(48, 48),
  activation = "relu",
  dropout_prob = 0.147,
  learning_rate = 0.067,
  L2_reg = 16.094,
  L1_reg = 0.0,
  momentum = 0.863,
  optimizer = "adam",
  lr_decay = 0.0006494,
  standardize = TRUE
)

# --------------------------
# Train model (fixed optimizer loop)
# --------------------------
model$train_model(list(
  x = torch_tensor(x_train, dtype = torch_float()),
  t = torch_tensor(t_train, dtype = torch_float()),
  e = torch_tensor(e_train, dtype = torch_float())
), n_epochs = 50, verbose = TRUE)

# --------------------------
# Compute bootstrap C-index
# --------------------------
cindex_res <- bootstrap_cindex(model, x_test, t_test, e_test, n_boot = 500)
print(sprintf("C-index: %.4f, 95%% CI: %.4f - %.4f",
              cindex_res$c_index, cindex_res$ci95[1], cindex_res$ci95[2]))
