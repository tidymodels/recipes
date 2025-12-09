# Check if all columns are present

`check_cols()` creates a *specification* of a recipe step that will
check if all the columns of the training frame are present in the new
data.

## Usage

``` r
check_cols(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("cols")
)
```

## Arguments

- recipe:

  A recipe object. The check will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this check. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this check since no new variables are created.

- trained:

  A logical for whether the selectors in `...` have been resolved by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- skip:

  A logical. Should the check be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this check to identify it.

## Value

An updated version of `recipe` with the new check added to the sequence
of any existing operations.

## Details

This check will break the
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
function if any of the specified columns is not present in the data. If
the check passes, nothing is changed to the data.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this check, a tibble with columns `terms` (the selectors or variables
selected) and `value` (the type) is returned.

## See also

Other checks:
[`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md),
[`check_missing()`](https://recipes.tidymodels.org/dev/reference/check_missing.md),
[`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md),
[`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_rec <- recipe(HHV ~ ., data = biomass) |>
  step_rm(sample, dataset) |>
  check_cols(contains("gen")) |>
  step_center(all_numeric_predictors())
if (FALSE) { # \dontrun{
bake(biomass_rec, biomass[, c("carbon", "HHV")])
} # }
```
