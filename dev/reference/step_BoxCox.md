# Box-Cox transformation for non-negative data

`step_BoxCox()` creates a *specification* of a recipe step that will
transform data using a Box-Cox transformation.

## Usage

``` r
step_BoxCox(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  lambdas = NULL,
  limits = c(-5, 5),
  num_unique = 5,
  skip = FALSE,
  id = rand_id("BoxCox")
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

  An integer to specify minimum required unique values to evaluate for a
  transformation.

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

The Box-Cox transformation, which requires a strictly positive variable,
can be used to rescale a variable to be more similar to a normal
distribution. In this package, the partial log-likelihood function is
directly optimized within a reasonable set of transformation values
(which can be changed by the user).

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

Sakia, R. M. (1992). The Box-Cox transformation technique: A review.
*The Statistician*, 169-178..

## See also

Other individual transformation steps:
[`step_YeoJohnson()`](https://recipes.tidymodels.org/dev/reference/step_YeoJohnson.md),
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
rec <- recipe(~., data = as.data.frame(state.x77))

bc_trans <- step_BoxCox(rec, all_numeric())

bc_estimates <- prep(bc_trans, training = as.data.frame(state.x77))
#> Warning: Non-positive values in selected variable.
#> Warning: No Box-Cox transformation could be estimated for: `Frost`.

bc_data <- bake(bc_estimates, as.data.frame(state.x77))

plot(density(state.x77[, "Illiteracy"]), main = "before")

plot(density(bc_data$Illiteracy), main = "after")


tidy(bc_trans, number = 1)
#> # A tibble: 1 × 3
#>   terms         value id          
#>   <chr>         <dbl> <chr>       
#> 1 all_numeric()    NA BoxCox_rKPRd
tidy(bc_estimates, number = 1)
#> # A tibble: 7 × 3
#>   terms          value id          
#>   <chr>          <dbl> <chr>       
#> 1 Population  0.000966 BoxCox_rKPRd
#> 2 Income      0.524    BoxCox_rKPRd
#> 3 Illiteracy -0.379    BoxCox_rKPRd
#> 4 Life Exp    4.59     BoxCox_rKPRd
#> 5 Murder      0.606    BoxCox_rKPRd
#> 6 HS Grad     1.92     BoxCox_rKPRd
#> 7 Area        0.250    BoxCox_rKPRd
```
