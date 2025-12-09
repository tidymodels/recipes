# Remove observations with missing values

`step_naomit()` creates a *specification* of a recipe step that will
remove observations (rows of data) if they contain `NA` or `NaN` values.

## Usage

``` r
step_naomit(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = TRUE,
  id = rand_id("naomit")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Unused, include for consistency with other steps.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated. Again included for consistency.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = FALSE`.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Row Filtering

This step can entirely remove observations (rows of data), which can
have unintended and/or problematic consequences when applying the step
to new data later via
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
Consider whether `skip = TRUE` or `skip = FALSE` is more appropriate in
any given use case. In most instances that affect the rows of the data
being predicted, this step probably should not be applied at all;
instead, execute operations like this outside and before starting a
preprocessing
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
recipe(Ozone ~ ., data = airquality) |>
  step_naomit(Solar.R) |>
  prep(airquality, verbose = FALSE) |>
  bake(new_data = NULL)
#> # A tibble: 146 × 6
#>    Solar.R  Wind  Temp Month   Day Ozone
#>      <int> <dbl> <int> <int> <int> <int>
#>  1     190   7.4    67     5     1    41
#>  2     118   8      72     5     2    36
#>  3     149  12.6    74     5     3    12
#>  4     313  11.5    62     5     4    18
#>  5     299   8.6    65     5     7    23
#>  6      99  13.8    59     5     8    19
#>  7      19  20.1    61     5     9     8
#>  8     194   8.6    69     5    10    NA
#>  9     256   9.7    69     5    12    16
#> 10     290   9.2    66     5    13    11
#> # ℹ 136 more rows
```
