# B-spline basis functions

`step_bs()` creates a *specification* of a recipe step that will create
new columns that are basis expansions of variables using B-splines.

## Usage

``` r
step_bs(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  deg_free = NULL,
  degree = 3,
  objects = NULL,
  options = list(),
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("bs")
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

  The degrees of freedom for the spline. As the degrees of freedom for a
  spline increase, more flexible and complex curves can be generated.
  When a single degree of freedom is used, the result is a rescaled
  version of the original data.

- degree:

  Degree of polynomial spline (integer).

- objects:

  A list of [`splines::bs()`](https://rdrr.io/r/splines/bs.html) objects
  created once the step has been trained.

- options:

  A list of options for
  [`splines::bs()`](https://rdrr.io/r/splines/bs.html) which should not
  include `x`, `degree`, or `df`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

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

`step_bs()` can create new features from a single variable that enable
fitting routines to model this variable in a nonlinear manner. The
extent of the possible nonlinearity is determined by the `df`, `degree`,
or `knots` arguments of
[`splines::bs()`](https://rdrr.io/r/splines/bs.html). The original
variables are removed from the data and new columns are added. The
naming convention for the new variables is `varname_bs_1` and so on.

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

- `deg_free`: Spline Degrees of Freedom (type: integer, default: NULL)

- `degree`: Polynomial Degree (type: integer, default: 3)

## Case weights

The underlying operation does not allow for case weights.

## See also

Other individual transformation steps:
[`step_BoxCox()`](https://recipes.tidymodels.org/dev/reference/step_BoxCox.md),
[`step_YeoJohnson()`](https://recipes.tidymodels.org/dev/reference/step_YeoJohnson.md),
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

with_splines <- rec |>
  step_bs(carbon, hydrogen)
with_splines <- prep(with_splines, training = biomass_tr)

expanded <- bake(with_splines, biomass_te)
expanded
#> # A tibble: 80 × 10
#>    oxygen nitrogen sulfur   HHV carbon_bs_1 carbon_bs_2 carbon_bs_3
#>     <dbl>    <dbl>  <dbl> <dbl>       <dbl>       <dbl>       <dbl>
#>  1   47.2     0.3    0.22  18.3       0.437      0.273      0.0568 
#>  2   48.1     2.85   0.34  17.6       0.444      0.236      0.0417 
#>  3   49.1     2.4    0.3   17.2       0.444      0.229      0.0394 
#>  4   37.3     1.8    0.5   18.9       0.437      0.273      0.0571 
#>  5   42.8     0.2    0     20.5       0.427      0.301      0.0707 
#>  6   41.7     0.7    0.2   18.5       0.442      0.248      0.0465 
#>  7   54.1     1.19   0.51  15.1       0.440      0.184      0.0256 
#>  8   33.8     0.95   0.2   16.2       0.444      0.222      0.0369 
#>  9   31.1     0.14   4.9   11.1       0.359      0.0771     0.00552
#> 10   23.7     4.63   1.05  10.8       0.338      0.0643     0.00408
#> # ℹ 70 more rows
#> # ℹ 3 more variables: hydrogen_bs_1 <dbl>, hydrogen_bs_2 <dbl>,
#> #   hydrogen_bs_3 <dbl>
```
