# Logit transformation

`step_logit()` creates a *specification* of a recipe step that will
logit transform the data.

## Usage

``` r
step_logit(
  recipe,
  ...,
  offset = 0,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("logit")
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

- offset:

  A numeric value to modify values of the columns that are either one or
  zero. They are modified to be `x - offset` or `offset`, respectively.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

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
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

The logit transformation takes values between zero and one and
translates them to be on the real line using the function
`f(p) = log(p/(1-p))`.

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

## See also

Other individual transformation steps:
[`step_BoxCox()`](https://recipes.tidymodels.org/dev/reference/step_BoxCox.md),
[`step_YeoJohnson()`](https://recipes.tidymodels.org/dev/reference/step_YeoJohnson.md),
[`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md),
[`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md),
[`step_hyperbolic()`](https://recipes.tidymodels.org/dev/reference/step_hyperbolic.md),
[`step_inverse()`](https://recipes.tidymodels.org/dev/reference/step_inverse.md),
[`step_invlogit()`](https://recipes.tidymodels.org/dev/reference/step_invlogit.md),
[`step_log()`](https://recipes.tidymodels.org/dev/reference/step_log.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md),
[`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md),
[`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md),
[`step_relu()`](https://recipes.tidymodels.org/dev/reference/step_relu.md),
[`step_sqrt()`](https://recipes.tidymodels.org/dev/reference/step_sqrt.md)

## Examples

``` r
set.seed(313)
examples <- matrix(runif(40), ncol = 2)
examples <- data.frame(examples)

rec <- recipe(~ X1 + X2, data = examples)

logit_trans <- rec |>
  step_logit(all_numeric_predictors())

logit_obj <- prep(logit_trans, training = examples)

transformed_te <- bake(logit_obj, examples)
plot(examples$X1, transformed_te$X1)


tidy(logit_trans, number = 1)
#> # A tibble: 1 × 2
#>   terms                    id         
#>   <chr>                    <chr>      
#> 1 all_numeric_predictors() logit_ooyvr
tidy(logit_obj, number = 1)
#> # A tibble: 2 × 2
#>   terms id         
#>   <chr> <chr>      
#> 1 X1    logit_ooyvr
#> 2 X2    logit_ooyvr
```
