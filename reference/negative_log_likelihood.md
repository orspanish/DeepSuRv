# Compute Negative Log-Likelihood

Compute Negative Log-Likelihood

## Usage

``` r
negative_log_likelihood(model, X, E)
```

## Arguments

- E:

  A numeric vector of event indicators (1 = event occurred, 0 =
  censored).

- self:

  A model-like object containing a method that returns the predicted log
  hazard.

- deterministic:

  Logical; if TRUE, calculates deterministic network outputs.

## Value

A single numeric value representing the negative partial log-likelihood.

## Examples

``` r
fake_self <- list(risk = function(deterministic) c(0.1, 0.2, 0.3))
E <- c(1, 0, 1)
negative_log_likelihood(fake_self, E)
#> Error in model$risk(torch_tensor(as.matrix(X), dtype = torch_float()))$squeeze: $ operator is invalid for atomic vectors
```
