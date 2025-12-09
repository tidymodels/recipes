# Near-zero variance filter

`step_nzv()` creates a *specification* of a recipe step that will
potentially remove variables that are highly sparse and unbalanced.

## Usage

``` r
step_nzv(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  freq_cut = 95/5,
  unique_cut = 10,
  options = list(freq_cut = 95/5, unique_cut = 10),
  removals = NULL,
  skip = FALSE,
  id = rand_id("nzv")
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

- freq_cut, unique_cut:

  Numeric parameters for the filtering process. See the Details section
  below.

- options:

  A list of options for the filter (see Details below).

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

This step diagnoses predictors that have one unique value (i.e. are zero
variance predictors) or predictors that have both of the following
characteristics:

1.  they have very few unique values relative to the number of samples
    and

2.  the ratio of the frequency of the most common value to the frequency
    of the second most common value is large.

For example, an example of near-zero variance predictor is one that, for
1000 samples, has two distinct values and 999 of them are a single
value.

To be flagged, first, the frequency of the most prevalent value over the
second most frequent value (called the "frequency ratio") must be above
`freq_cut`. Secondly, the "percent of unique values," the number of
unique values divided by the total number of samples (times 100), must
also be below `unique_cut`.

In the above example, the frequency ratio is 999 and the unique value
percent is 0.2%.

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

- `freq_cut`: Frequency Distribution Ratio (type: double, default: 95/5)

- `unique_cut`: % Unique Values (type: double, default: 10)

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other variable filter steps:
[`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md),
[`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md),
[`step_lincomb()`](https://recipes.tidymodels.org/dev/reference/step_lincomb.md),
[`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass$sparse <- c(1, rep(0, nrow(biomass) - 1))

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen +
  nitrogen + sulfur + sparse,
data = biomass_tr
)

nzv_filter <- rec |>
  step_nzv(all_predictors())

filter_obj <- prep(nzv_filter, training = biomass_tr)

filtered_te <- bake(filter_obj, biomass_te)
any(names(filtered_te) == "sparse")
#> [1] FALSE

tidy(nzv_filter, number = 1)
#> # A tibble: 1 × 2
#>   terms            id       
#>   <chr>            <chr>    
#> 1 all_predictors() nzv_rHEnN
tidy(filter_obj, number = 1)
#> # A tibble: 1 × 2
#>   terms  id       
#>   <chr>  <chr>    
#> 1 sparse nzv_rHEnN
```
