# Minimal tests for Parts 1–3 using synthetic data + mock model

test_that("setup: explainer can be created from mock model and toy data", {
  set.seed(1)

  # mock DeepSuRv model: numeric risk from row sums
  mock_model <- list(predict_risk = function(nd) rowSums(as.matrix(nd)) * 0.01)
  class(mock_model) <- "DeepSuRv"

  # tiny toy dataset
  df <- data.frame(
    x1 = rnorm(8),
    x2 = rnorm(8),
    time  = rexp(8, rate = 0.2),
    event = sample(c(0, 1), 8, replace = TRUE)
  )

  expl <- make_deepsurv_explainer(mock_model, df, time_col = "time", event_col = "event")

  expect_true(is.list(expl))
  expect_true(all(c("data", "y", "predict_function", "predict_survival_function") %in% names(expl)))
  expect_true(is.data.frame(expl$data))

  # keep objects for subsequent tests
  assign("expl", expl, envir = .GlobalEnv)
  assign("x_row", df[1, c("x1","x2"), drop = FALSE], envir = .GlobalEnv)
})

test_that("Part 1: feature ranking and single-feature counterfactual run", {
  skip_if_not(exists("expl", .GlobalEnv))
  x1 <- get("x_row", .GlobalEnv)
  expl <- get("expl", .GlobalEnv)

  topk <- cf_top_features(expl, x1, k = 2, eps = 0.05)
  expect_s3_class(topk, "data.frame")
  expect_true(all(c("feature","delta_abs_risk") %in% names(topk)))
  expect_true(nrow(topk) <= 2)
  expect_gte(topk$delta_abs_risk[1], 0)

  cf_out <- cf_compare_one(expl, x1, feature = colnames(expl$data)[1], change = -0.10)
  expect_true(is.list(cf_out))
  expect_true(all(c("times","S_orig","S_cf","risk_orig","risk_cf","x_cf") %in% names(cf_out)))
  expect_length(cf_out$S_orig, length(cf_out$times))
  expect_length(cf_out$S_cf,   length(cf_out$times))

  # plotting should not error
  expect_silent(cf_plot(cf_out))
})

test_that("Part 2: multi-feature perturbation with constraints works", {
  skip_if_not(exists("x_row", .GlobalEnv))
  x1 <- get("x_row", .GlobalEnv)

  cons <- list(x1 = list(min = -5, max = 5), x2 = list(min = -5, max = 5))
  x_cf <- cf_perturb_multi(x1, changes = c(x1 = -0.10, x2 = 0.20), constraints = cons)

  expect_s3_class(x_cf, "data.frame")
  expect_true(all(colnames(x_cf) == colnames(x1)))
  expect_true(all(x_cf$x1 >= -5 & x_cf$x1 <= 5))
  expect_true(all(x_cf$x2 >= -5 & x_cf$x2 <= 5))
})

test_that("Part 3: batch CF curve generation and stability check run", {
  skip_if_not(exists("expl", .GlobalEnv) && exists("x_row", .GlobalEnv))
  expl <- get("expl", .GlobalEnv)
  x1   <- get("x_row", .GlobalEnv)

  scenarios <- list(
    A = c(x1 = -0.10, x2 = -0.05),
    B = c(x1 =  0.05)
  )
  res <- cf_generate_curves(expl, x1, scenarios)
  expect_true(is.list(res))
  expect_true(all(names(scenarios) %in% names(res)))
  expect_true(all(c("times","S_orig","S_cf","risk_orig","risk_cf","x_cf") %in% names(res$A)))

  st <- cf_stability(expl, x1, base_change = c(x1 = -0.10), n = 5, jitter = 0.02)
  expect_true(is.numeric(st))
  expect_equal(names(st), c("mean","sd"))
  expect_false(any(is.na(st)))
})
