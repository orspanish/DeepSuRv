# Format a DeepSurv Dataset into a Data Frame

Converts a dataset list containing `x`, `t`, and `e` components into a
single data frame with covariates, duration, and event indicator
columns. Optionally renames a treatment column.

## Usage

``` r
format_dataset_to_df(dataset, duration_col, event_col, trt_idx = NULL)
```

## Arguments

- dataset:

  A list containing matrices `x`, `t`, and `e`.

- duration_col:

  Integer; column index of the duration variable in `dataset$t`.

- event_col:

  Integer; column index of the event indicator in `dataset$e`.

- trt_idx:

  Optional integer index in `dataset$x` that should be renamed to
  `"treat"`.

## Value

A data frame containing all covariates plus `dt` (duration) and `censor`
(event indicator).
