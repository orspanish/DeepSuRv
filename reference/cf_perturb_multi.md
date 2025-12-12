# Apply multi-feature relative perturbations

Modifies multiple features by given relative changes (e.g., +0.1 = +10%)
and reapplies constraints.

## Usage

``` r
cf_perturb_multi(x_row, changes, constraints = NULL)
```

## Arguments

- x_row:

  A one-row data frame of covariates.

- changes:

  A named numeric vector of relative changes (e.g., c(age = -0.1,
  cholesterol = -0.2)).

- constraints:

  Optional constraints list passed to cf_apply_constraints().

## Value

A modified data frame with updated feature values.

## Examples

``` r
# cf_perturb_multi(patient, c(age = -0.1, bmi = 0.05))
```
