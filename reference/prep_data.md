# Prepare data for DeepSurv training

This function preprocesses a dataset for use in DeepSurv. It removes
incomplete cases, scales numeric variables, converts character/factor
variables to numeric encodings, constructs training data torch tensors,
and orders observations by decreasing survival time.

## Usage

``` r
prep_data(data, time_col, event_col)
```

## Arguments

- data:

  A `data.frame` containing covariates and survival outcome variables.

- time_col:

  A string giving the name of the survival time column.

- event_col:

  A string giving the name of the event indicator column (typically 1 =
  event, 0 = censored).

## Value

A list containing:

- X_mat:

  A processed version of the covariate matrix with numeric scaling and
  factor/character conversion applied.

- dat:

  A matrix of the processed covariate data with the addition of the time
  and event data.

## Examples

``` r
if (FALSE) { # \dontrun{
train_obj <- prep_data(mydata, time_col = "time", event_col = "status")
} # }
```
