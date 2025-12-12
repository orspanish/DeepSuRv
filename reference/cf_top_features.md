# Identify top influential features for an individual prediction

Perturbs each feature slightly and measures the change in predicted
risk. Ranks features by their absolute effect on the output.

## Usage

``` r
cf_top_features(expl, x_row, k = 3, eps = 0.05)
```

## Arguments

- expl:

  A Survex explainer object created by make_deepsurv_explainer().

- x_row:

  A one-row data frame representing a single individual.

- k:

  Integer; number of top features to return (default = 3).

- eps:

  Numeric; small relative change applied to each feature (default =
  0.05).

## Value

A data frame containing the top influential features and their absolute
risk changes.

## Examples

``` r
# cf_top_features(expl, patient_data[1, ])
```
