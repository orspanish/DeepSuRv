test_that("DeepSuRv initializes and returns forward output of correct shape", {

  model <- DeepSuRv(
    n_in = 3,
    hidden_layers = c(4, 2),
    activation = "relu",
    dropout = NULL,
    batch_norm = FALSE,
    standardize = FALSE
  )

  x <- torch::torch_randn(5, 3)  # 5 samples, 3 features

  out <- model$forward(x)

  expect_s3_class(model, "nn_module")
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.numeric(out$shape), c(5, 1))
})

test_that("risk() is equivalent to forward()", {
  model <- DeepSuRv(n_in = 2, hidden_layers = c(4))

  x <- torch::torch_randn(10, 2)

  r1 <- model$risk(x)
  r2 <- model$forward(x)

  expect_true(inherits(r1, "torch_tensor"))
  expect_true(inherits(r2, "torch_tensor"))

  expect_equal(as.numeric(r1$shape), as.numeric(r2$shape))
  expect_true(torch::torch_allclose(r1, r2))
})

test_that("predict_risk() produces numeric output", {
  model <- DeepSuRv(n_in = 2, hidden_layers = c(4))

  x <- matrix(rnorm(20), ncol = 2)  # 10 samples
  risks <- model$predict_risk(x)

  expect_type(risks, "double")
  expect_length(risks, 10)
})

test_that("predict_risk() is deterministic in eval mode", {
  model <- DeepSuRv(n_in = 2, hidden_layers = c(3), dropout = 0.5)

  x <- matrix(rnorm(8), nrow = 4)

  model$eval()  # turn off dropout

  p1 <- model$predict_risk(x)
  p2 <- model$predict_risk(x)

  expect_equal(p1, p2)
})
