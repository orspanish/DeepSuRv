# Compare original and counterfactual survival for one feature

Generates a counterfactual scenario by modifying a single feature (e.g.,
-10%) and recomputing the survival curve and risk score.

## Usage

``` r
cf_compare_one(expl, x_row, feature, change = -0.1)
```

## Arguments

- expl:

  A Survex explainer object.

- x_row:

  A one-row data frame of covariates.

- feature:

  Character; the feature name to modify.

- change:

  Numeric; relative change (e.g., -0.10 = -10%).

## Value

A list containing:

- times:

  Vector of time points.

- S_orig:

  Original survival curve.

- S_cf:

  Counterfactual survival curve.

- risk_orig:

  Original risk score.

- risk_cf:

  Counterfactual risk score.

- x_cf:

  Modified data frame after change.

## Examples

``` r
# cf_compare_one(expl, patient_data[1, ], "cholesterol", change = -0.1)
```
