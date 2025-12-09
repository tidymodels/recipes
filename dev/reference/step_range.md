# Scaling numeric data to a specific range

`step_range()` creates a *specification* of a recipe step that will
normalize numeric data to be within a pre-defined range of values.

## Usage

``` r
step_range(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  min = 0,
  max = 1,
  clipping = TRUE,
  ranges = NULL,
  skip = FALSE,
  id = rand_id("range")
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

- min, max:

  Single numeric values for the smallest (or largest) value in the
  transformed data.

- clipping:

  A single logical value for determining whether application of
  transformation onto new data should be forced to be inside `min` and
  `max`. Defaults to TRUE.

- ranges:

  A character vector of variables that will be normalized. Note that
  this is ignored until the values are determined by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
  Setting this value will be ineffective.

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

When a new data point is outside of the ranges seen in the training set,
the new values are truncated at `min` or `max`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `min`, `max` , and
`id`:

- terms:

  character, the selectors or variables selected

- min:

  numeric, lower range

- max:

  numeric, upper range

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other normalization steps:
[`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md),
[`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md),
[`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

ranged_trans <- rec |>
  step_range(carbon, hydrogen)

ranged_obj <- prep(ranged_trans, training = biomass_tr)

transformed_te <- bake(ranged_obj, biomass_te)

biomass_te[1:10, names(transformed_te)]
#>    carbon hydrogen oxygen nitrogen sulfur    HHV
#> 15  46.35     5.67  47.20     0.30   0.22 18.275
#> 20  43.25     5.50  48.06     2.85   0.34 17.560
#> 26  42.70     5.50  49.10     2.40   0.30 17.173
#> 31  46.40     6.10  37.30     1.80   0.50 18.851
#> 36  48.76     6.32  42.77     0.20   0.00 20.547
#> 41  44.30     5.50  41.70     0.70   0.20 18.467
#> 46  38.94     5.23  54.13     1.19   0.51 15.095
#> 51  42.10     4.66  33.80     0.95   0.20 16.240
#> 55  29.20     4.40  31.10     0.14   4.90 11.147
#> 65  27.80     3.77  23.69     4.63   1.05 10.750
transformed_te
#> # A tibble: 80 × 6
#>    carbon hydrogen oxygen nitrogen sulfur   HHV
#>     <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>
#>  1  0.384    0.490   47.2     0.3    0.22  18.3
#>  2  0.347    0.475   48.1     2.85   0.34  17.6
#>  3  0.340    0.475   49.1     2.4    0.3   17.2
#>  4  0.385    0.527   37.3     1.8    0.5   18.9
#>  5  0.414    0.546   42.8     0.2    0     20.5
#>  6  0.360    0.475   41.7     0.7    0.2   18.5
#>  7  0.295    0.451   54.1     1.19   0.51  15.1
#>  8  0.333    0.402   33.8     0.95   0.2   16.2
#>  9  0.177    0.379   31.1     0.14   4.9   11.1
#> 10  0.160    0.325   23.7     4.63   1.05  10.8
#> # ℹ 70 more rows

tidy(ranged_trans, number = 1)
#> # A tibble: 2 × 4
#>   terms      min   max id         
#>   <chr>    <dbl> <dbl> <chr>      
#> 1 carbon      NA    NA range_80cUH
#> 2 hydrogen    NA    NA range_80cUH
tidy(ranged_obj, number = 1)
#> # A tibble: 2 × 4
#>   terms      min   max id         
#>   <chr>    <dbl> <dbl> <chr>      
#> 1 carbon   14.6   97.2 range_80cUH
#> 2 hydrogen  0.03  11.6 range_80cUH
```
