# Removes original columns if options apply

This helper function should be used whenever the argument
`keep_original_cols` is used in a function.

## Usage

``` r
remove_original_cols(new_data, object, col_names)
```

## Arguments

- new_data:

  A tibble.

- object:

  A step object.

- col_names:

  A character vector, denoting columns to remove.

## Value

new_data with `col_names` removed if
`get_keep_original_cols(object) == TRUE` or `object$preserve == TRUE`.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
