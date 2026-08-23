# Transform columns in place

This helper function applies `fn` to each of `col_names` and assigns the
results back into `new_data` in bulk. It should be used in
`bake.step_*()` functions instead of assigning to `new_data[[col_name]]`
inside a `for` loop, which is quadratic in the number of columns and
becomes prohibitively slow for wide data.

## Usage

``` r
recipes_map_cols(new_data, col_names, fn)
```

## Arguments

- new_data:

  A tibble.

- col_names:

  A character vector, denoting columns to transform.

- fn:

  A function taking one to three arguments: the column to transform, the
  position of that column within `col_names`, and its name. Only the
  arguments that `fn` accepts are passed, so `\(x) x * 2`,
  `\(x, i) x - means[[i]]`, and `\(x, i, col_name) ...` are all valid.
  It must return a vector the same length as the column it is given.

## Value

`new_data` with `col_names` transformed by `fn`.

## Details

Use the position `i` to look up per-column values estimated during
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), after
aligning them to `col_names` once outside of `fn`. Do not use `col_name`
to index a named vector with one element per selected column, as the
repeated name matching is what makes the naive approach slow. `col_name`
is intended for error messages and for lookups into small objects.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
