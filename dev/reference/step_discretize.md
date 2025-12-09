# Discretize Numeric Variables

`step_discretize()` creates a *specification* of a recipe step that will
convert numeric data into a factor with bins having approximately the
same number of data points (based on a training set).

## Usage

``` r
step_discretize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  num_breaks = 4,
  min_unique = 10,
  objects = NULL,
  options = list(prefix = "bin"),
  skip = FALSE,
  id = rand_id("discretize")
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

- num_breaks:

  An integer defining how many cuts to make of the data.

- min_unique:

  An integer defining a sample size line of dignity for the binning. If
  (the number of unique values)`/(cuts+1)` is less than `min_unique`, no
  discretization takes place.

- objects:

  The
  [`discretize()`](https://recipes.tidymodels.org/dev/reference/discretize.md)
  objects are stored here once the recipe has be trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- options:

  A list of options to
  [`discretize()`](https://recipes.tidymodels.org/dev/reference/discretize.md).
  A default is set for the argument `x`. Note that using the options
  `prefix` and `labels` when more than one variable is being transformed
  might be problematic as all variables inherit those values.

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

Note that missing values will be turned into a factor level with the
level `prefix_missing`, where `prefix` is specified in the `options`
argument.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the breaks

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `min_unique`: Unique Value Threshold (type: integer, default: 10)

- `num_breaks`: Number of Cut Points (type: integer, default: 4)

## Case weights

The underlying operation does not allow for case weights.

## See also

Other discretization steps:
[`step_cut()`](https://recipes.tidymodels.org/dev/reference/step_cut.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
) |>
  step_discretize(carbon, hydrogen)

rec <- prep(rec, biomass_tr)
#> Warning: Note that the options `prefix` and `labels` will be applied to all
#> variables.
binned_te <- bake(rec, biomass_te)
table(binned_te$carbon)
#> 
#> bin1 bin2 bin3 bin4 
#>   22   17   25   16 

tidy(rec, 1)
#> # A tibble: 10 × 3
#>    terms      value id              
#>    <chr>      <dbl> <chr>           
#>  1 carbon   -Inf    discretize_ONQD4
#>  2 carbon     44.7  discretize_ONQD4
#>  3 carbon     47.1  discretize_ONQD4
#>  4 carbon     49.7  discretize_ONQD4
#>  5 carbon    Inf    discretize_ONQD4
#>  6 hydrogen -Inf    discretize_ONQD4
#>  7 hydrogen    5.20 discretize_ONQD4
#>  8 hydrogen    5.78 discretize_ONQD4
#>  9 hydrogen    6.05 discretize_ONQD4
#> 10 hydrogen  Inf    discretize_ONQD4
```
