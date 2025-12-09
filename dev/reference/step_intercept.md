# Add intercept (or constant) column

`step_intercept()` creates a *specification* of a recipe step that will
add an intercept or constant term in the first column of a data matrix.
`step_intercept()` defaults to *predictor* role so that it is by default
only called in the bake step. Be careful to avoid unintentional
transformations when calling steps with
[`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md).

## Usage

``` r
step_intercept(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  name = "intercept",
  value = 1L,
  skip = FALSE,
  id = rand_id("intercept")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  Argument ignored; included for consistency with other step
  specification functions.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated. Again included only for consistency.

- name:

  Character name for newly added column

- value:

  A numeric constant to fill the intercept column. Defaults to `1L`.

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)
rec_trans <- recipe(HHV ~ ., data = biomass_tr[, -(1:2)]) |>
  step_intercept(value = 2) |>
  step_scale(carbon)

rec_obj <- prep(rec_trans, training = biomass_tr)

with_intercept <- bake(rec_obj, biomass_te)
with_intercept
#> # A tibble: 80 × 7
#>    intercept carbon hydrogen oxygen nitrogen sulfur   HHV
#>        <dbl>  <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>
#>  1         2   4.45     5.67   47.2     0.3    0.22  18.3
#>  2         2   4.16     5.5    48.1     2.85   0.34  17.6
#>  3         2   4.10     5.5    49.1     2.4    0.3   17.2
#>  4         2   4.46     6.1    37.3     1.8    0.5   18.9
#>  5         2   4.68     6.32   42.8     0.2    0     20.5
#>  6         2   4.26     5.5    41.7     0.7    0.2   18.5
#>  7         2   3.74     5.23   54.1     1.19   0.51  15.1
#>  8         2   4.04     4.66   33.8     0.95   0.2   16.2
#>  9         2   2.81     4.4    31.1     0.14   4.9   11.1
#> 10         2   2.67     3.77   23.7     4.63   1.05  10.8
#> # ℹ 70 more rows
```
