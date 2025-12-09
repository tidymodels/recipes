# Ratio variable creation

`step_ratio()` creates a *specification* of a recipe step that will
create one or more ratios from selected numeric variables.

## Usage

``` r
step_ratio(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  denom = denom_vars(),
  naming = function(numer, denom) {
     make.names(paste(numer, denom, sep = "_o_"))
 },
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("ratio")
)

denom_vars(...)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which variables will be used
  in the *numerator* of the ratio. When used with `denom_vars`, the dots
  indicate which variables are used in the *denominator*. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- denom:

  Bare names that specifies which variables are used in the denominator
  that can include specific variable names separated by commas or
  different selectors (see
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)).
  Can also be a strings or tidyselect for backwards compatibility If a
  column is included in both lists to be numerator and denominator, it
  will be removed from the listing.

- naming:

  A function that defines the naming convention for new ratio columns.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `TRUE`.

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `denom` , and
`id`:

- terms:

  character, the selectors or variables selected

- denom:

  character, name of denominator selected

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other multivariate transformation steps:
[`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
[`step_classdist_shrunken()`](https://recipes.tidymodels.org/dev/reference/step_classdist_shrunken.md),
[`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
[`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md),
[`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md),
[`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md),
[`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md),
[`step_kpca_poly()`](https://recipes.tidymodels.org/dev/reference/step_kpca_poly.md),
[`step_kpca_rbf()`](https://recipes.tidymodels.org/dev/reference/step_kpca_rbf.md),
[`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
[`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md),
[`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
[`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
library(recipes)
data(biomass, package = "modeldata")

biomass$total <- apply(biomass[, 3:7], 1, sum)
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
  sulfur + total,
data = biomass_tr
)

ratio_recipe <- rec |>
  # all predictors over total
  step_ratio(all_numeric_predictors(), denom = total,
             keep_original_cols = FALSE)

ratio_recipe <- prep(ratio_recipe, training = biomass_tr)

ratio_data <- bake(ratio_recipe, biomass_te)
ratio_data
#> # A tibble: 80 × 6
#>      HHV carbon_o_total hydrogen_o_total oxygen_o_total
#>    <dbl>          <dbl>            <dbl>          <dbl>
#>  1  18.3          0.465           0.0568          0.473
#>  2  17.6          0.432           0.055           0.481
#>  3  17.2          0.427           0.055           0.491
#>  4  18.9          0.504           0.0662          0.405
#>  5  20.5          0.497           0.0645          0.436
#>  6  18.5          0.479           0.0595          0.451
#>  7  15.1          0.389           0.0523          0.541
#>  8  16.2          0.515           0.0570          0.414
#>  9  11.1          0.419           0.0631          0.446
#> 10  10.8          0.456           0.0619          0.389
#> # ℹ 70 more rows
#> # ℹ 2 more variables: nitrogen_o_total <dbl>, sulfur_o_total <dbl>
```
