# Prepare Dataset for Survival Analysis

This function prepares a dataset for survival modeling by optionally
standardizing covariates and sorting the data by survival time in
descending order. It is useful for preprocessing before fitting models
such as Cox proportional hazards or DeepSurv.

## Usage

``` r
prepare_data(dataset, standardize = TRUE, offset = NULL, scale = NULL)
```

## Arguments

- dataset:

  A list containing the survival data with elements:

  x

  :   A numeric matrix or data frame of covariates (predictors).

  e

  :   A numeric or integer vector of event indicators (1 = event, 0 =
      censored).

  t

  :   A numeric vector of survival times.

- standardize:

  Logical; if `TRUE` (default), the covariates in `x` are standardized.

- offset:

  Optional numeric vector giving the means of the covariates used for
  standardization. If `NULL`, they are computed from `x`.

- scale:

  Optional numeric vector giving the standard deviations of the
  covariates used for standardization. If `NULL`, they are computed from
  `x`.

## Value

A list with the following elements:

- x:

  The (optionally) standardized and sorted covariate matrix.

- e:

  The sorted event indicator vector.

- t:

  The sorted survival times.

- offset:

  The mean values used for standardization.

- scale:

  The standard deviations used for standardization.

## Details

If `standardize = TRUE`, the function calls
[`standardize_x()`](https://orspanish.github.io/DeepSuRv/reference/standardize_x.md)
to center and scale each column of `x`. The function then sorts all
observations by survival time (`t`) in decreasing order to ensure
correct ordering for likelihood-based methods.

## See also

[`standardize_x`](https://orspanish.github.io/DeepSuRv/reference/standardize_x.md)
for the helper function used in standardization.

## Examples

``` r
dataset <- list(
  x = matrix(rnorm(20), ncol = 2),
  e = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1),
  t = runif(10, 1, 10)
)
result <- prepare_data(dataset)
str(result)
#> List of 5
#>  $ x     : num [1:10, 1:2] -0.292 -0.319 0.588 1.908 0.614 ...
#>  $ e     : num [1:10] 0 1 0 1 1 0 1 1 0 1
#>  $ t     : num [1:10] 9.53 8.43 6.37 6.13 5.9 ...
#>  $ offset: num [1:2] -0.179 -0.124
#>  $ scale : num [1:2] 1.18 1.05
```
