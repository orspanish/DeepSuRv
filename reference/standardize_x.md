# Standardize a Covariate Matrix

Internal helper function that centers and scales each column of a
matrix. If `offset` (column means) and `scale` (column SDs) are not
provided, they are computed from the data.

## Usage

``` r
standardize_x(x, offset = NULL, scale = NULL)
```

## Arguments

- x:

  A numeric matrix of covariates.

- offset:

  Optional numeric vector of column means to subtract. If `NULL`, column
  means are computed from `x`.

- scale:

  Optional numeric vector of column standard deviations to divide by. If
  `NULL`, column SDs are computed from `x`. Any zero SD is replaced with
  1 to avoid division-by-zero.

## Value

A list with components:

- `x`:

  The standardized matrix.

- `offset`:

  The column means used for centering.

- `scale`:

  The column SDs used for scaling.
