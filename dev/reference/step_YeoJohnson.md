# Yeo-Johnson transformation

`step_YeoJohnson()` creates a *specification* of a recipe step that will
transform data using a Yeo-Johnson transformation.

## Usage

``` r
step_YeoJohnson(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  lambdas = NULL,
  limits = c(-5, 5),
  num_unique = 5,
  na_rm = TRUE,
  skip = FALSE,
  id = rand_id("YeoJohnson")
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

- lambdas:

  A numeric vector of transformation values. This is `NULL` until
  computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- limits:

  A length 2 numeric vector defining the range to compute the
  transformation parameter lambda.

- num_unique:

  An integer where data that have less possible values will not be
  evaluated for a transformation.

- na_rm:

  A logical value indicating whether `NA` values should be removed
  during computations.

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

The Yeo-Johnson transformation is very similar to the Box-Cox but does
not require the input variables to be strictly positive. In the package,
the partial log-likelihood function is directly optimized within a
reasonable set of transformation values (which can be changed by the
user).

This transformation is typically done on the outcome variable using the
residuals for a statistical model (such as ordinary least squares).
Here, a simple null model (intercept only) is used to apply the
transformation to the *predictor* variables individually. This can have
the effect of making the variable distributions more symmetric.

If the transformation parameters are estimated to be very closed to the
bounds, or if the optimization fails, a value of `NA` is used and no
transformation is applied.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the lambda estimate

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## References

Yeo, I. K., and Johnson, R. A. (2000). A new family of power
transformations to improve normality or symmetry. *Biometrika*.

## See also

Other individual transformation steps:
[`step_BoxCox()`](https://recipes.tidymodels.org/dev/reference/step_BoxCox.md),
[`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md),
[`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md),
[`step_hyperbolic()`](https://recipes.tidymodels.org/dev/reference/step_hyperbolic.md),
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
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

yj_transform <- step_YeoJohnson(rec, all_numeric())

yj_estimates <- prep(yj_transform, training = biomass_tr)

yj_te <- bake(yj_estimates, biomass_te)

plot(density(biomass_te$sulfur), main = "before")

plot(density(yj_te$sulfur), main = "after")


tidy(yj_transform, number = 1)
#> # A tibble: 1 × 3
#>   terms         value id              
#>   <chr>         <dbl> <chr>           
#> 1 all_numeric()    NA YeoJohnson_Z4qwE
tidy(yj_estimates, number = 1)
#> # A tibble: 6 × 3
#>   terms      value id              
#>   <chr>      <dbl> <chr>           
#> 1 carbon   -0.0225 YeoJohnson_Z4qwE
#> 2 hydrogen  2.10   YeoJohnson_Z4qwE
#> 3 oxygen    1.78   YeoJohnson_Z4qwE
#> 4 nitrogen -0.830  YeoJohnson_Z4qwE
#> 5 sulfur   -4.09   YeoJohnson_Z4qwE
#> 6 HHV      -0.388  YeoJohnson_Z4qwE
```
