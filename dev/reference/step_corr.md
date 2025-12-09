# High correlation filter

`step_corr()` creates a *specification* of a recipe step that will
potentially remove variables that have large absolute correlations with
other variables.

## Usage

``` r
step_corr(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  use = "pairwise.complete.obs",
  method = "pearson",
  removals = NULL,
  skip = FALSE,
  id = rand_id("corr")
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

  A value for the threshold of absolute correlation values. The step
  will try to remove the minimum number of columns so that all the
  resulting absolute correlations are less than this value.

- use:

  A character string for the `use` argument to the
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) function.

- method:

  A character string for the `method` argument to the
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) function.

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

This step attempts to remove variables to keep the largest absolute
correlation between the variables less than `threshold`.

The filter tries to prioritize predictors for removal based on the
global affect on the overall correlation structure. If you have two
identical predictors, the variable ordered first will be removed.

When a column has a single unique value, that column will be excluded
from the correlation analysis. Also, if the data set has sporadic
missing values (and an inappropriate value of `use` is chosen), some
columns will also be excluded from the filter.

The arguments `use` and `method` don't take effect if case weights are
used in the recipe.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected to be removed

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `threshold`: Threshold (type: double, default: 0.9)

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other variable filter steps:
[`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md),
[`step_lincomb()`](https://recipes.tidymodels.org/dev/reference/step_lincomb.md),
[`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md),
[`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

## Author

Original R code for filtering algorithm by Dong Li, modified by Max
Kuhn. Contributions by Reynald Lescarbeau (for original in `caret`
package). Max Kuhn for the `step` function.

## Examples

``` r
data(biomass, package = "modeldata")

set.seed(3535)
biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + duplicate,
  data = biomass_tr
)

corr_filter <- rec |>
  step_corr(all_numeric_predictors(), threshold = .5)

filter_obj <- prep(corr_filter, training = biomass_tr)

filtered_te <- bake(filter_obj, biomass_te)
round(abs(cor(biomass_tr[, c(3:7, 9)])), 2)
#>           carbon hydrogen oxygen nitrogen sulfur duplicate
#> carbon      1.00     0.32   0.63     0.15   0.09      1.00
#> hydrogen    0.32     1.00   0.54     0.07   0.19      0.31
#> oxygen      0.63     0.54   1.00     0.18   0.31      0.63
#> nitrogen    0.15     0.07   0.18     1.00   0.27      0.15
#> sulfur      0.09     0.19   0.31     0.27   1.00      0.10
#> duplicate   1.00     0.31   0.63     0.15   0.10      1.00
round(abs(cor(filtered_te)), 2)
#>           hydrogen nitrogen sulfur duplicate  HHV
#> hydrogen      1.00     0.11   0.26      0.20 0.10
#> nitrogen      0.11     1.00   0.16      0.13 0.11
#> sulfur        0.26     0.16   1.00      0.13 0.08
#> duplicate     0.20     0.13   0.13      1.00 0.94
#> HHV           0.10     0.11   0.08      0.94 1.00

tidy(corr_filter, number = 1)
#> # A tibble: 1 × 2
#>   terms                    id        
#>   <chr>                    <chr>     
#> 1 all_numeric_predictors() corr_ubc7G
tidy(filter_obj, number = 1)
#> # A tibble: 2 × 2
#>   terms  id        
#>   <chr>  <chr>     
#> 1 oxygen corr_ubc7G
#> 2 carbon corr_ubc7G
```
