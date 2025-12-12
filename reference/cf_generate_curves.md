# Generate counterfactual survival curves for multiple scenarios

Compute survival curves and risk scores under multiple counterfactual
scenarios. Each scenario represents a set of feature perturbations.

## Usage

``` r
cf_generate_curves(expl, x_row, change_list, constraints = NULL)
```

## Arguments

- expl:

  A Survex explainer object created by make_deepsurv_explainer().

- x_row:

  A one-row data frame of covariates representing a single individual.

- change_list:

  A named list, where each element is a named numeric vector of relative
  changes for selected features (e.g., list(age = -0.1, bmi = 0.05)).

- constraints:

  Optional named list specifying min/max bounds, passed to
  cf_apply_constraints().

## Value

A named list where each element contains:

- times:

  Vector of time points used for survival curves.

- S_orig:

  Original survival curve.

- S_cf:

  Counterfactual survival curve.

- risk_orig:

  Original risk score.

- risk_cf:

  Counterfactual risk score.

- x_cf:

  Modified covariate data after perturbation.

## Examples

``` r
# cf_generate_curves(expl, patient[1, ],
#   list(low_bp = c(bp = -0.2), high_bmi = c(bmi = 0.1)))
```
