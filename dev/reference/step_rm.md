# General variable filter

`step_rm()` creates a *specification* of a recipe step that will remove
selected variables.

## Usage

``` r
step_rm(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  removals = NULL,
  skip = FALSE,
  id = rand_id("rm")
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

- removals:

  A character string that contains the names of columns that should be
  removed. These values are not determined until
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  called.

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

This step can potentially remove columns from the data set. This may
cause issues for subsequent steps in your recipe if the missing columns
are specifically referenced by name. To avoid this, see the advice in
the *Tips for saving recipes and filtering columns* section of
[selections](https://recipes.tidymodels.org/dev/reference/selections.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other variable filter steps:
[`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md),
[`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md),
[`step_lincomb()`](https://recipes.tidymodels.org/dev/reference/step_lincomb.md),
[`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

library(dplyr)
smaller_set <- rec |>
  step_rm(contains("gen"))

smaller_set <- prep(smaller_set, training = biomass_tr)

filtered_te <- bake(smaller_set, biomass_te)
filtered_te
#> # A tibble: 80 × 3
#>    carbon sulfur   HHV
#>     <dbl>  <dbl> <dbl>
#>  1   46.4   0.22  18.3
#>  2   43.2   0.34  17.6
#>  3   42.7   0.3   17.2
#>  4   46.4   0.5   18.9
#>  5   48.8   0     20.5
#>  6   44.3   0.2   18.5
#>  7   38.9   0.51  15.1
#>  8   42.1   0.2   16.2
#>  9   29.2   4.9   11.1
#> 10   27.8   1.05  10.8
#> # ℹ 70 more rows

tidy(smaller_set, number = 1)
#> # A tibble: 3 × 2
#>   terms    id      
#>   <chr>    <chr>   
#> 1 hydrogen rm_bpu3Q
#> 2 oxygen   rm_bpu3Q
#> 3 nitrogen rm_bpu3Q
```
