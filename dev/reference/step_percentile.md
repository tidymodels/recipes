# Percentile transformation

`step_percentile()` creates a *specification* of a recipe step that
replaces the value of a variable with its percentile from the training
set.

## Usage

``` r
step_percentile(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ref_dist = NULL,
  options = list(probs = (0:100)/100),
  outside = "none",
  skip = FALSE,
  id = rand_id("percentile")
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

- ref_dist:

  The computed percentiles is stored here once this preprocessing step
  has be trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- options:

  A named list of options to pass to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html). See
  Details for more information.

- outside:

  A character, describing how interpolation is to take place outside the
  interval `[min(x), max(x)]`. `none` means nothing will happen and
  values outside the range will be `NA`. `lower` means that new values
  less than `min(x)` will be given the value `0`. `upper` means that new
  values larger than `max(x)` will be given the value `1`. `both` will
  handle both cases. Defaults to `none`.

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
this step, a tibble is returned with columns `terms`, `value`,
`percentile` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the value at the percentile

- percentile:

  numeric, the percentile as a percentage

- id:

  character, id of this step

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

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
) |>
  step_percentile(carbon)

prepped_rec <- prep(rec)

prepped_rec |>
  bake(biomass_te)
#> # A tibble: 80 × 6
#>    carbon hydrogen oxygen nitrogen sulfur   HHV
#>     <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>
#>  1 0.421      5.67   47.2     0.3    0.22  18.3
#>  2 0.18       5.5    48.1     2.85   0.34  17.6
#>  3 0.156      5.5    49.1     2.4    0.3   17.2
#>  4 0.423      6.1    37.3     1.8    0.5   18.9
#>  5 0.666      6.32   42.8     0.2    0     20.5
#>  6 0.218      5.5    41.7     0.7    0.2   18.5
#>  7 0.0803     5.23   54.1     1.19   0.51  15.1
#>  8 0.139      4.66   33.8     0.95   0.2   16.2
#>  9 0.0226     4.4    31.1     0.14   4.9   11.1
#> 10 0.0178     3.77   23.7     4.63   1.05  10.8
#> # ℹ 70 more rows

tidy(rec, 1)
#> # A tibble: 1 × 4
#>   terms  value percentile id              
#>   <chr>  <dbl>      <dbl> <chr>           
#> 1 carbon    NA         NA percentile_PRBJI
tidy(prepped_rec, 1)
#> # A tibble: 101 × 4
#>    terms  value percentile id              
#>    <chr>  <dbl>      <dbl> <chr>           
#>  1 carbon  14.6          0 percentile_PRBJI
#>  2 carbon  25.9          1 percentile_PRBJI
#>  3 carbon  28.4          2 percentile_PRBJI
#>  4 carbon  31.6          3 percentile_PRBJI
#>  5 carbon  35.1          4 percentile_PRBJI
#>  6 carbon  35.9          5 percentile_PRBJI
#>  7 carbon  37.5          6 percentile_PRBJI
#>  8 carbon  38.3          7 percentile_PRBJI
#>  9 carbon  38.9          8 percentile_PRBJI
#> 10 carbon  39.6          9 percentile_PRBJI
#> # ℹ 91 more rows
```
