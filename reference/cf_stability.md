# Assess stability of counterfactual predictions

Evaluates how sensitive model risk predictions are to small random
perturbations. Jitter around a base change and report risk mean/sd

## Usage

``` r
cf_stability(expl, x_row, base_change, n = 10, jitter = 0.02, seed = 1)
```

## Arguments

- expl:

  A Survex explainer object.

- x_row:

  A one-row data frame of covariates.

- base_change:

  A named numeric vector of base relative changes.

- n:

  Integer; number of random samples (default = 10).

- jitter:

  Numeric; maximum random deviation added to each change (default =
  0.02).

- seed:

  Integer; random seed for reproducibility (default = 1).

## Value

A named numeric vector with mean and standard deviation of predicted
risks under jittered perturbations.

## Examples

``` r
# cf_stability(expl, patient[1, ], c(bmi = 0.05))
```
