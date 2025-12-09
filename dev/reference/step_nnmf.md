# Non-negative matrix factorization signal extraction

`step_nnmf()` creates a *specification* of a recipe step that will
convert numeric data into one or more non-negative components.

**\[deprecated\]**

Please use
[`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md)
instead of this step function.

## Usage

``` r
step_nnmf(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  num_run = 30,
  options = list(),
  res = NULL,
  columns = NULL,
  prefix = "NNMF",
  seed = sample.int(10^5, 1),
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("nnmf")
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

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- num_comp:

  The number of components to retain as new predictors. If `num_comp` is
  greater than the number of columns or the number of possible
  components, a smaller value will be used. If `num_comp = 0` is set
  then no transformation is done and selected variables will stay
  unchanged, regardless of the value of `keep_original_cols`.

- num_run:

  A positive integer for the number of computations runs used to obtain
  a consensus projection.

- options:

  A list of options to `nmf()` in the NMF package by way of the `NNMF()`
  function in the `dimRed` package. **Note** that the arguments `data`
  and `ndim` should not be passed here, and that NMF's parallel
  processing is turned off in favor of resample-level parallelization.

- res:

  The `NNMF()` object is stored here once this preprocessing step has
  been trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. See notes below.

- seed:

  An integer that will be used to set the seed in isolation when
  computing the factorization.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

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

Non-negative matrix factorization computes latent components that have
non-negative values and take into account that the original data have
non-negative values.

The argument `num_comp` controls the number of components that will be
retained (the original variables that are used to derive the components
are removed from the data). The new components will have names that
begin with `prefix` and a sequence of numbers. The variable names are
padded with zeros. For example, if `num_comp < 10`, their names will be
`NNMF1` - `NNMF9`. If `num_comp = 101`, the names would be `NNMF1` -
`NNMF101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`,
`component` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, value of loading

- component:

  character, name of component

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `num_comp`: \# Components (type: integer, default: 2)

- `num_run`: Number of Computation Runs (type: integer, default: 30)

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
[`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
[`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
data(biomass, package = "modeldata")

# rec <- recipe(HHV ~ ., data = biomass) |>
#   update_role(sample, new_role = "id var") |>
#   update_role(dataset, new_role = "split variable") |>
#   step_nnmf(all_numeric_predictors(), num_comp = 2, seed = 473, num_run = 2) |>
#   prep(training = biomass)
#
# bake(rec, new_data = NULL)
#
# library(ggplot2)
# bake(rec, new_data = NULL) |>
#  ggplot(aes(x = NNMF2, y = NNMF1, col = HHV)) + geom_point()
```
