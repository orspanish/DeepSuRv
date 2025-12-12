# Load HDF5 Datasets into R

Internal helper function that reads a DeepSurv-style HDF5 file
containing groups such as `"train"`, `"valid"`, and `"test"`. Each group
is expected to contain datasets such as `"x"`, `"t"`, and `"e"`.

## Usage

``` r
load_datasets(dataset_file)
```

## Arguments

- dataset_file:

  Path to an HDF5 dataset file.

## Value

A named list where each element corresponds to a dataset group, and each
group contains named arrays read from the HDF5 file.

## Details

Uses
[`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)
to open the file in read-only mode. Reads each dataset fully into memory
using the `[]`-operator.
