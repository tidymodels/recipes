# Impute numeric data below the threshold of measurement

`step_impute_lower()` creates a *specification* of a recipe step
designed for cases where the non-negative numeric data cannot be
measured below a known value. In these cases, one method for imputing
the data is to substitute the truncated value by a random uniform number
between zero and the truncation point.

## Usage

``` r
step_impute_lower(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = NULL,
  skip = FALSE,
  id = rand_id("impute_lower")
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

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- threshold:

  A named numeric vector of lower bounds. This is `NULL` until computed
  by [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

`step_impute_lower()` estimates the variable minimums from the data used
in the `training` argument of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) then
simulates a value for any data at the minimum with a random uniform
value between zero and the minimum.

As of `recipes` 0.1.16, this function name changed from
[`step_lowerimpute()`](https://recipes.tidymodels.org/dev/reference/step_lowerimpute.md)
to `step_impute_lower()`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the estimated value

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other imputation steps:
[`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md),
[`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md),
[`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md),
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
[`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md)

## Examples

``` r
library(recipes)
data(biomass, package = "modeldata")

## Truncate some values to emulate what a lower limit of
## the measurement system might look like

biomass$carbon <- ifelse(biomass$carbon > 40, biomass$carbon, 40)
biomass$hydrogen <- ifelse(biomass$hydrogen > 5, biomass$carbon, 5)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

impute_rec <- rec |>
  step_impute_lower(carbon, hydrogen)

tidy(impute_rec, number = 1)
#> # A tibble: 2 × 3
#>   terms    value id                
#>   <chr>    <dbl> <chr>             
#> 1 carbon      NA impute_lower_CM3q3
#> 2 hydrogen    NA impute_lower_CM3q3

impute_rec <- prep(impute_rec, training = biomass_tr)

tidy(impute_rec, number = 1)
#> # A tibble: 2 × 3
#>   terms    value id                
#>   <chr>    <dbl> <chr>             
#> 1 carbon      40 impute_lower_CM3q3
#> 2 hydrogen     5 impute_lower_CM3q3

transformed_te <- bake(impute_rec, biomass_te)

plot(transformed_te$carbon, biomass_te$carbon,
  ylab = "pre-imputation", xlab = "imputed"
)
```
