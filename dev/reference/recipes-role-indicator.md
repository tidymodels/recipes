# Role indicators

This helper function is meant to be used in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) methods
to identify predictors and outcomes by names.

## Usage

``` r
recipes_names_predictors(info)

recipes_names_outcomes(info)
```

## Arguments

- info:

  data.frame with variable information of columns.

## Value

Character vector of column names.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
