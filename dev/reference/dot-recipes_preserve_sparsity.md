# Does step destroy sparsity of columns

Does step destroy sparsity of columns

## Usage

``` r
.recipes_preserve_sparsity(x, ...)
```

## Arguments

- x:

  An object.

## Value

Single logical.

## Details

This function return `TRUE` if modifications by this step is expected to
destroy sparsity of its columns. It does not know whether it will happen
or not since it doesn't know if the step will select sparse columns or
not.

It returns `FALSE` for steps that have been modified to handle sparse
columns correctly.
