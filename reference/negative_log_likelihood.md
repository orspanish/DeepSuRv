# Compute Negative Log-Likelihood

Compute Negative Log-Likelihood

## Usage

``` r
negative_log_likelihood(model, X, E)
```

## Arguments

- model:

  A model-like object containing a method that returns the predicted log
  hazard.

- X:

  A numeric matrix or data frame of input features, with one row per
  subject and one column per feature

- E:

  A numeric vector of event indicators (1 = event occurred, 0 =
  censored).

## Value

A single numeric value representing the negative partial log-likelihood.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a fake model object with a risk() method
fake_self <- list(
  risk = function(x) {
    # Return a torch tensor of log-risk values,
    # one per row of the input feature matrix
    torch::torch_tensor(c(0.1, 0.2, 0.3))
  }
)

# Feature matrix: one row per subject
X <- data.frame(
  age = c(60, 55, 70),
  biomarker = c(1.2, 0.7, 2.1)
)

# Event indicator (1 = event, 0 = censored)
E <- c(1, 0, 1)

# Compute negative partial log-likelihood
negative_log_likelihood(fake_self, X, E)
} # }
```
