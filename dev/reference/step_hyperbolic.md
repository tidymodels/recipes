# Hyperbolic transformations

`step_hyperbolic()` creates a *specification* of a recipe step that will
transform data using a hyperbolic function.

## Usage

``` r
step_hyperbolic(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  func = c("sinh", "cosh", "tanh"),
  inverse = TRUE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("hyperbolic")
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

- func:

  A character value for the function. Valid values are `"sinh"`,
  `"cosh"`, or `"tanh"`.

- inverse:

  A logical: should the inverse function be used?

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `inverse`, `func`
, and `id`:

- terms:

  character, the selectors or variables selected

- inverse:

  logical, is the inverse function be used

- func:

  character, name of function. `"sinh"`, `"cosh"`, or `"tanh"`

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
[`step_inverse()`](https://recipes.tidymodels.org/dev/reference/step_inverse.md),
[`step_invlogit()`](https://recipes.tidymodels.org/dev/reference/step_invlogit.md),
[`step_log()`](https://recipes.tidymodels.org/dev/reference/step_log.md),
[`step_logit()`](https://recipes.tidymodels.org/dev/reference/step_logit.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md),
[`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md),
[`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md),
[`step_relu()`](https://recipes.tidymodels.org/dev/reference/step_relu.md),
[`step_sqrt()`](https://recipes.tidymodels.org/dev/reference/step_sqrt.md)

## Examples

``` r
set.seed(313)
examples <- matrix(rnorm(40), ncol = 2)
examples <- as.data.frame(examples)

rec <- recipe(~ V1 + V2, data = examples)

cos_trans <- rec |>
  step_hyperbolic(
    all_numeric_predictors(),
    func = "cosh", inverse = FALSE
  )

cos_obj <- prep(cos_trans, training = examples)

transformed_te <- bake(cos_obj, examples)
plot(examples$V1, transformed_te$V1)


tidy(cos_trans, number = 1)
#> # A tibble: 1 × 4
#>   terms                    inverse func  id              
#>   <chr>                    <lgl>   <chr> <chr>           
#> 1 all_numeric_predictors() FALSE   cosh  hyperbolic_IhS7o
tidy(cos_obj, number = 1)
#> # A tibble: 2 × 4
#>   terms inverse func  id              
#>   <chr> <lgl>   <chr> <chr>           
#> 1 V1    FALSE   cosh  hyperbolic_IhS7o
#> 2 V2    FALSE   cosh  hyperbolic_IhS7o
```
