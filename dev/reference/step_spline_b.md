# Basis splines

`step_spline_b()` creates a *specification* of a recipe step that
creates b-spline features.

## Usage

``` r
step_spline_b(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  deg_free = 10,
  degree = 3,
  complete_set = FALSE,
  options = NULL,
  keep_original_cols = FALSE,
  results = NULL,
  skip = FALSE,
  id = rand_id("spline_b")
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

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- deg_free:

  The degrees of freedom for the b-spline. As the degrees of freedom for
  a b-spline increase, more flexible and complex curves can be
  generated.

- degree:

  A non-negative integer specifying the degree of the piece-wise
  polynomial. The default value is 3 for cubic splines. Zero degree is
  allowed for piece-wise constant basis functions.

- complete_set:

  If `TRUE`, the complete basis matrix will be returned. Otherwise, the
  first basis will be excluded from the output. This maps to the
  `intercept` argument of the corresponding function from the splines2
  package and has the same default value.

- options:

  A list of options for
  [`splines2::bSpline()`](https://wwenjie.org/splines2/reference/bSpline.html)
  which should not include `x`, `df`, `degree`, or `intercept`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

- results:

  A list of objects created once the step has been trained.

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

An object with classes `"step_spline_b"` and `"step"`.

## Details

Spline transformations take a numeric column and create multiple
features that, when used in a model, can estimate nonlinear trends
between the column and some outcome. The degrees of freedom determines
how many new features are added to the data.

Setting `periodic = TRUE` in the list passed to `options`, a periodic
version of the spline is used.

If the spline expansion fails for a selected column, the step will
remove that column's results (but will retain the original data). Use
the [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method to
determine which columns were used.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `deg_free`: Spline Degrees of Freedom (type: integer, default: 10)

- `degree`: Polynomial Degree (type: integer, default: 3)

## Case weights

The underlying operation does not allow for case weights.

## See also

[`splines2::bSpline()`](https://wwenjie.org/splines2/reference/bSpline.html)

## Examples

``` r
library(tidyr)
library(dplyr)

library(ggplot2)
data(ames, package = "modeldata")

spline_rec <- recipe(Sale_Price ~ Longitude, data = ames) |>
  step_spline_b(Longitude, deg_free = 6, keep_original_cols = TRUE) |>
  prep()

tidy(spline_rec, number = 1)
#> # A tibble: 1 × 2
#>   terms     id            
#>   <chr>     <chr>         
#> 1 Longitude spline_b_TpMy9

# Show where each feature is active
spline_rec |>
  bake(new_data =  NULL,-Sale_Price) |>
  pivot_longer(c(starts_with("Longitude_")), names_to = "feature", values_to = "value") |>
  mutate(feature = gsub("Longitude_", "feature ", feature)) |>
  filter(value > 0) |>
  ggplot(aes(x = Longitude, y = value)) +
  geom_line() +
  facet_wrap(~ feature)
```
