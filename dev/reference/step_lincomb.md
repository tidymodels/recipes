# Linear combination filter

`step_lincomb()` creates a *specification* of a recipe step that will
potentially remove numeric variables that have exact linear combinations
between them.

## Usage

``` r
step_lincomb(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  max_steps = 5,
  removals = NULL,
  skip = FALSE,
  id = rand_id("lincomb")
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

- max_steps:

  The number of times to apply the algorithm.

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

This step finds exact linear combinations between two or more variables
and recommends which column(s) should be removed to resolve the issue.
This algorithm may need to be applied multiple times (as defined by
`max_steps`).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other variable filter steps:
[`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md),
[`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md),
[`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md),
[`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

## Author

Max Kuhn, Kirk Mettler, and Jed Wing

## Examples

``` r
data(biomass, package = "modeldata")

biomass$new_1 <- with(
  biomass,
  .1 * carbon - .2 * hydrogen + .6 * sulfur
)
biomass$new_2 <- with(
  biomass,
  .5 * carbon - .2 * oxygen + .6 * nitrogen
)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
  sulfur + new_1 + new_2,
data = biomass_tr
)

lincomb_filter <- rec |>
  step_lincomb(all_numeric_predictors())

lincomb_filter_trained <- prep(lincomb_filter, training = biomass_tr)
lincomb_filter_trained
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 7
#> 
#> ── Training information 
#> Training data contained 456 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Linear combination filter removed: new_1 new_2 | Trained

tidy(lincomb_filter, number = 1)
#> # A tibble: 1 × 2
#>   terms                    id           
#>   <chr>                    <chr>        
#> 1 all_numeric_predictors() lincomb_jrHEn
tidy(lincomb_filter_trained, number = 1)
#> # A tibble: 2 × 2
#>   terms id           
#>   <chr> <chr>        
#> 1 new_1 lincomb_jrHEn
#> 2 new_2 lincomb_jrHEn
```
