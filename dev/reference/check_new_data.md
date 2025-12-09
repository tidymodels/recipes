# Check for required column at bake-time

When baking a step, create an information error message when a column
that is used by the step is not present in `new_data`.

## Usage

``` r
check_new_data(req, object, new_data)
```

## Arguments

- req:

  A character vector of required columns.

- object:

  A step object.

- new_data:

  A tibble of data being baked.

## Value

Invisible NULL. Side effects are the focus of the function.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
