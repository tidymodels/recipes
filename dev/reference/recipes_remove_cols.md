# Removes columns if options apply

This helper function removes columns based on character vectors.

## Usage

``` r
recipes_remove_cols(new_data, object, col_names = character())
```

## Arguments

- new_data:

  A tibble.

- object:

  A step object.

- col_names:

  A character vector, denoting columns to remove. Will overwrite
  `object$removals` if set.

## Value

`new_data` with column names removed if specified by `col_names` or
`object$removals`.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
