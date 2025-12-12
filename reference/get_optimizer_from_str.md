# Map string to torch optimizer function

Returns the corresponding torch optimizer class given a string.

## Usage

``` r
get_optimizer_from_str(opt)
```

## Arguments

- opt:

  Character. One of "sgd", "adam", or "rmsprop".

## Value

A function or NULL if the string is unrecognized.
