# Create a survex explainer object for a trained DeepSuRv model

Create a survex explainer object for a trained DeepSuRv model

## Usage

``` r
make_deepsurv_explainer(model, data, time_col, event_col)
```

## Arguments

- model:

  A trained DeepSuRv model.

- data:

  A data.frame containing covariates + survival columns.

- time_col:

  Name of survival time variable.

- event_col:

  Name of event indicator (1 = event).

## Value

A survex explainer object.
