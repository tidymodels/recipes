# Zero variance filter

`step_zv()` creates a *specification* of a recipe step that will remove
variables that contain only a single value.

## Usage

``` r
step_zv(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  group = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("zv")
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

- group:

  An optional character string or call to
  [`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html)
  that can be used to specify a group(s) within which to identify
  variables that contain only a single value. If the grouping variables
  are contained in terms selector, they will not be considered for
  removal.

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

  character, names of the columns that will be removed

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
[`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass$one_value <- 1

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen +
  nitrogen + sulfur + one_value,
data = biomass_tr
)

zv_filter <- rec |>
  step_zv(all_predictors())

filter_obj <- prep(zv_filter, training = biomass_tr)

filtered_te <- bake(filter_obj, biomass_te)
any(names(filtered_te) == "one_value")
#> [1] FALSE

tidy(zv_filter, number = 1)
#> # A tibble: 1 × 2
#>   terms            id      
#>   <chr>            <chr>   
#> 1 all_predictors() zv_IAmGJ
tidy(filter_obj, number = 1)
#> # A tibble: 1 × 2
#>   terms     id      
#>   <chr>     <chr>   
#> 1 one_value zv_IAmGJ
```
