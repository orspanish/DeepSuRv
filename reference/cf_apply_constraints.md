# Apply biological constraints to feature values

Ensures feature values remain within user-defined min/max ranges to
maintain biological plausibility.

## Usage

``` r
cf_apply_constraints(x_row, constraints = NULL)
```

## Arguments

- x_row:

  A one-row data frame of covariates.

- constraints:

  A named list, where each element is itself a list possibly containing
  min and/or max values (e.g.list(age = list(min = 0, max = 120))).

## Value

A constrained data frame with adjusted feature values.

## Examples

``` r
# cf_apply_constraints(patient, list(age = list(min = 0, max = 100)))
```
