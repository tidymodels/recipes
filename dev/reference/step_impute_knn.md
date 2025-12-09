# Impute via k-nearest neighbors

`step_impute_knn()` creates a *specification* of a recipe step that will
impute missing data using nearest neighbors.

## Usage

``` r
step_impute_knn(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  neighbors = 5,
  impute_with = all_predictors(),
  options = list(nthread = 1, eps = 1e-08),
  ref_data = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("impute_knn")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables to be imputed. When
  used with `imp_vars`, these dots indicate which variables are used to
  predict the missing data in each variable. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- neighbors:

  The number of neighbors.

- impute_with:

  Bare names or selectors functions that specify which variables are
  used to impute the variables that can include specific variable names
  separated by commas or different selectors (see
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)).
  If a column is included in both lists to be imputed and to be an
  imputation predictor, it will be removed from the latter and not used
  to impute itself.

- options:

  A named list of options to pass to
  [`gower::gower_topn()`](https://rdrr.io/pkg/gower/man/gower_topn.html).
  Available options are currently `nthread` and `eps`.

- ref_data:

  A tibble of data that will reflect the data preprocessing done up to
  the point of this imputation step. This is `NULL` until the step is
  trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

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

The step uses the training set to impute any other data sets. The only
distance function available is Gower's distance which can be used for
mixtures of nominal and numeric data.

Once the nearest neighbors are determined, the mode is used to predictor
nominal variables and the mean is used for numeric data. Note that, if
the underlying data are integer, the mean will be converted to an
integer too.

Note that if a variable that is to be imputed is also in `impute_with`,
this variable will be ignored.

It is possible that missing values will still occur after imputation if
a large majority (or all) of the imputing variables are also missing.

As of `recipes` 0.1.16, this function name changed from
[`step_knnimpute()`](https://recipes.tidymodels.org/dev/reference/step_knnimpute.md)
to `step_impute_knn()`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `predictors`,
`neighbors` , and `id`:

- terms:

  character, the selectors or variables selected

- predictors:

  character, selected predictors used to impute

- neighbors:

  integer, number of neighbors

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `neighbors`: \# Nearest Neighbors (type: integer, default: 5)

## Case weights

The underlying operation does not allow for case weights.

## References

Gower, C. (1971) "A general coefficient of similarity and some of its
properties," Biometrics, 857-871.

## See also

Other imputation steps:
[`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md),
[`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md),
[`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md),
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
[`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md)

## Examples

``` r
library(recipes)
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]
biomass_te_whole <- biomass_te

# induce some missing data at random
set.seed(9039)
carb_missing <- sample(1:nrow(biomass_te), 3)
nitro_missing <- sample(1:nrow(biomass_te), 3)

biomass_te$carbon[carb_missing] <- NA
biomass_te$nitrogen[nitro_missing] <- NA

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

ratio_recipe <- rec |>
  step_impute_knn(all_predictors(), neighbors = 3)
ratio_recipe2 <- prep(ratio_recipe, training = biomass_tr)
imputed <- bake(ratio_recipe2, biomass_te)

# how well did it work?
summary(biomass_te_whole$carbon)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   27.80   44.24   47.30   47.96   49.00   79.34 
cbind(
  before = biomass_te_whole$carbon[carb_missing],
  after = imputed$carbon[carb_missing]
)
#>      before    after
#> [1,]  46.83 47.43000
#> [2,]  47.80 47.53333
#> [3,]  46.40 46.21000

summary(biomass_te_whole$nitrogen)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.010   0.295   0.690   1.092   1.450   4.790 
cbind(
  before = biomass_te_whole$nitrogen[nitro_missing],
  after = imputed$nitrogen[nitro_missing]
)
#>      before      after
#> [1,]   1.24 0.59333333
#> [2,]   0.30 0.92333333
#> [3,]   0.06 0.04666667

tidy(ratio_recipe, number = 1)
#> # A tibble: 1 × 4
#>   terms            predictors neighbors id              
#>   <chr>            <chr>          <dbl> <chr>           
#> 1 all_predictors() NA                 3 impute_knn_iyPXM
tidy(ratio_recipe2, number = 1)
#> # A tibble: 20 × 4
#>    terms    predictors neighbors id              
#>    <chr>    <chr>          <dbl> <chr>           
#>  1 carbon   hydrogen           3 impute_knn_iyPXM
#>  2 carbon   oxygen             3 impute_knn_iyPXM
#>  3 carbon   nitrogen           3 impute_knn_iyPXM
#>  4 carbon   sulfur             3 impute_knn_iyPXM
#>  5 hydrogen carbon             3 impute_knn_iyPXM
#>  6 hydrogen oxygen             3 impute_knn_iyPXM
#>  7 hydrogen nitrogen           3 impute_knn_iyPXM
#>  8 hydrogen sulfur             3 impute_knn_iyPXM
#>  9 oxygen   carbon             3 impute_knn_iyPXM
#> 10 oxygen   hydrogen           3 impute_knn_iyPXM
#> 11 oxygen   nitrogen           3 impute_knn_iyPXM
#> 12 oxygen   sulfur             3 impute_knn_iyPXM
#> 13 nitrogen carbon             3 impute_knn_iyPXM
#> 14 nitrogen hydrogen           3 impute_knn_iyPXM
#> 15 nitrogen oxygen             3 impute_knn_iyPXM
#> 16 nitrogen sulfur             3 impute_knn_iyPXM
#> 17 sulfur   carbon             3 impute_knn_iyPXM
#> 18 sulfur   hydrogen           3 impute_knn_iyPXM
#> 19 sulfur   oxygen             3 impute_knn_iyPXM
#> 20 sulfur   nitrogen           3 impute_knn_iyPXM
```
