# Extract transformed training set

**\[superseded\]**

As of `recipes` version 0.1.14, **`juice()` is superseded** in favor of
`bake(object, new_data = NULL)`.

As steps are estimated by `prep`, these operations are applied to the
training set. Rather than running
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) to
duplicate this processing, this function will return variables from the
processed training set.

## Usage

``` r
juice(object, ..., composition = "tibble")
```

## Arguments

- object:

  A `recipe` object that has been prepared with the option
  `retain = TRUE`.

- ...:

  One or more selector functions to choose which variables will be
  returned by the function. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details. If no selectors are given, the default is to use
  [`dplyr::everything()`](https://dplyr.tidyverse.org/reference/reexports.html).

- composition:

  Either `"tibble"`, `"matrix"`, `"data.frame"`, or
  ``` "dgCMatrix"``for the format of the processed data set. Also, note that this argument should be called **after** any selectors and the selectors should only resolve to numeric columns if  ```composition`is set to`"matrix"`or`"dgCMatrix"`. If the data contains sparse columns they will be perseved for `"tibble"`and`"data.frame"`, and efficiently used for `"dgCMatrix"\`.

## Details

`juice()` will return the results of a recipe where *all steps* have
been applied to the data, irrespective of the value of the step's `skip`
argument.

`juice()` can only be used if a recipe was prepped with `retain = TRUE`.
This is equivalent to `bake(object, new_data = NULL)` which is the
preferred way to extract the transformation of the training data set.

## See also

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
