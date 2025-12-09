# Missing value column filter

`step_filter_missing()` creates a *specification* of a recipe step that
will potentially remove variables that have too many missing values.

## Usage

``` r
step_filter_missing(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.1,
  removals = NULL,
  skip = FALSE,
  id = rand_id("filter_missing")
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

- threshold:

  A value for the threshold of missing values in column. The step will
  remove the columns where the proportion of missing values exceeds the
  threshold.

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

This step will remove variables if the proportion of missing values
exceeds the `threshold`.

All variables with missing values will be removed for `threshold = 0`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `threshold`: Threshold (type: double, default: 0.1)

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other variable filter steps:
[`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md),
[`step_lincomb()`](https://recipes.tidymodels.org/dev/reference/step_lincomb.md),
[`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md),
[`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

## Examples

``` r
data(credit_data, package = "modeldata")

rec <- recipe(Status ~ ., data = credit_data) |>
  step_filter_missing(all_predictors(), threshold = 0)

filter_obj <- prep(rec)

filtered_te <- bake(filter_obj, new_data = NULL)

tidy(rec, number = 1)
#> # A tibble: 1 × 2
#>   terms            id                  
#>   <chr>            <chr>               
#> 1 all_predictors() filter_missing_tkASb
tidy(filter_obj, number = 1)
#> # A tibble: 6 × 2
#>   terms   id                  
#>   <chr>   <chr>               
#> 1 Home    filter_missing_tkASb
#> 2 Marital filter_missing_tkASb
#> 3 Job     filter_missing_tkASb
#> 4 Income  filter_missing_tkASb
#> 5 Assets  filter_missing_tkASb
#> 6 Debt    filter_missing_tkASb
```
