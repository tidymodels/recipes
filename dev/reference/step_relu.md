# Apply (smoothed) rectified linear transformation

`step_relu()` creates a *specification* of a recipe step that will add
the rectified linear or softplus transformations of a variable to the
data set.

## Usage

``` r
step_relu(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  shift = 0,
  reverse = FALSE,
  smooth = FALSE,
  prefix = "right_relu_",
  columns = NULL,
  skip = FALSE,
  id = rand_id("relu")
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

- shift:

  A numeric value dictating a translation to apply to the data.

- reverse:

  A logical to indicate if the left hinge should be used as opposed to
  the right hinge.

- smooth:

  A logical indicating if the softplus function, a smooth approximation
  to the rectified linear transformation, should be used.

- prefix:

  A prefix for generated column names, defaults to "right_relu\_" for
  right hinge transformation and "left_relu\_" for reversed/left hinge
  transformations.

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

The rectified linear transformation is calculated as \$\$max(0, x -
c)\$\$ and is also known as the ReLu or right hinge function. If
`reverse` is true, then the transformation is reflected about the
y-axis, like so: \$\$max(0, c - x)\$\$ Setting the `smooth` option to
true will instead calculate a smooth approximation to ReLu according to
\$\$ln(1 + e^(x - c)\$\$ The `reverse` argument may also be applied to
this transformation.

## Connection to MARS:

The rectified linear transformation is used in Multivariate Adaptive
Regression Splines as a basis function to fit piecewise linear functions
to data in a strategy similar to that employed in tree based models. The
transformation is a popular choice as an activation function in many
neural networks, which could then be seen as a stacked generalization of
MARS when making use of ReLu activations. The hinge function also
appears in the loss function of Support Vector Machines, where it
penalizes residuals only if they are within a certain margin of the
decision boundary.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `shift`, `reverse`
, and `id`:

- terms:

  character, the selectors or variables selected

- shift:

  numeric, location of hinge

- reverse:

  logical, whether left hinge is used

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
[`step_logit()`](https://recipes.tidymodels.org/dev/reference/step_logit.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md),
[`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md),
[`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md),
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

transformed_te <- rec |>
  step_relu(carbon, shift = 40) |>
  prep(biomass_tr) |>
  bake(biomass_te)

transformed_te
#> # A tibble: 80 × 7
#>    carbon hydrogen oxygen nitrogen sulfur   HHV right_relu_carbon
#>     <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>             <dbl>
#>  1   46.4     5.67   47.2     0.3    0.22  18.3              6.35
#>  2   43.2     5.5    48.1     2.85   0.34  17.6              3.25
#>  3   42.7     5.5    49.1     2.4    0.3   17.2              2.70
#>  4   46.4     6.1    37.3     1.8    0.5   18.9              6.4 
#>  5   48.8     6.32   42.8     0.2    0     20.5              8.76
#>  6   44.3     5.5    41.7     0.7    0.2   18.5              4.30
#>  7   38.9     5.23   54.1     1.19   0.51  15.1              0   
#>  8   42.1     4.66   33.8     0.95   0.2   16.2              2.10
#>  9   29.2     4.4    31.1     0.14   4.9   11.1              0   
#> 10   27.8     3.77   23.7     4.63   1.05  10.8              0   
#> # ℹ 70 more rows
```
