# Partial least squares feature extraction

`step_pls()` creates a *specification* of a recipe step that will
convert numeric data into one or more new dimensions.

## Usage

``` r
step_pls(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  predictor_prop = 1,
  outcome = NULL,
  options = list(scale = TRUE),
  preserve = deprecated(),
  res = NULL,
  columns = NULL,
  prefix = "PLS",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("pls")
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

- predictor_prop:

  The maximum number of original predictors that can have non-zero
  coefficients for each PLS component (via regularization).

- outcome:

  When a single outcome is available, bare name, character strings or
  call to
  [`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html) can
  be used to specify a single outcome variable.

- options:

  A list of options to
  [`mixOmics::pls()`](https://rdrr.io/pkg/mixOmics/man/pls.html),
  [`mixOmics::spls()`](https://rdrr.io/pkg/mixOmics/man/spls.html),
  [`mixOmics::plsda()`](https://rdrr.io/pkg/mixOmics/man/plsda.html), or
  [`mixOmics::splsda()`](https://rdrr.io/pkg/mixOmics/man/splsda.html)
  (depending on the data and arguments).

- preserve:

  Use `keep_original_cols` instead to specify whether the original
  predictor data should be retained along with the new features.

- res:

  A list of results are stored here once this preprocessing step has
  been trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- prefix:

  A character string for the prefix of the resulting new variables. See
  notes below.

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

PLS is a supervised version of principal component analysis that
requires the outcome data to compute the new features.

This step requires the Bioconductor mixOmics package. If not installed,
the step will stop with a note about installing the package. Install
mixOmics using the pak package:

    # install.packages("pak")
    pak::pak("mixOmics")

The argument `num_comp` controls the number of components that will be
retained (the original variables that are used to derive the components
are removed from the data). The new components will have names that
begin with `prefix` and a sequence of numbers. The variable names are
padded with zeros. For example, if `num_comp < 10`, their names will be
`PLS1` - `PLS9`. If `num_comp = 101`, the names would be `PLS1` -
`PLS101`.

Sparsity can be encouraged using the `predictor_prop` parameter. This
affects each PLS component, and indicates the maximum proportion of
predictors with non-zero coefficients in each component. `step_pls()`
converts this proportion to determine the `keepX` parameter in
[`mixOmics::spls()`](https://rdrr.io/pkg/mixOmics/man/spls.html) and
[`mixOmics::splsda()`](https://rdrr.io/pkg/mixOmics/man/splsda.html).
See the references in
[`mixOmics::spls()`](https://rdrr.io/pkg/mixOmics/man/spls.html) for
details.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`,
`component` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, coefficients defined as \\W(P'W)^{-1}\\

- size:

  character, name of component

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `num_comp`: \# Components (type: integer, default: 2)

- `predictor_prop`: Proportion of Predictors (type: double, default: 1)

## Case weights

The underlying operation does not allow for case weights.

## References

<https://en.wikipedia.org/wiki/Partial_least_squares_regression>

Rohart F, Gautier B, Singh A, Lê Cao K-A (2017) *mixOmics: An R package
for 'omics feature selection and multiple data integration*. PLoS Comput
Biol 13(11): e1005752.
[doi:10.1371/journal.pcbi.1005752](https://doi.org/10.1371/journal.pcbi.1005752)

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
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
# requires the Bioconductor mixOmics package
data(biomass, package = "modeldata")

biom_tr <-
  biomass |>
  dplyr::filter(dataset == "Training") |>
  dplyr::select(-dataset, -sample)
biom_te <-
  biomass |>
  dplyr::filter(dataset == "Testing") |>
  dplyr::select(-dataset, -sample, -HHV)

dense_pls <-
  recipe(HHV ~ ., data = biom_tr) |>
  step_pls(all_numeric_predictors(), outcome = HHV, num_comp = 3)

sparse_pls <-
  recipe(HHV ~ ., data = biom_tr) |>
  step_pls(all_numeric_predictors(), outcome = HHV, num_comp = 3,
           predictor_prop = 4 / 5)

## -----------------------------------------------------------------------------
## PLS discriminant analysis

data(cells, package = "modeldata")

cell_tr <-
  cells |>
  dplyr::filter(case == "Train") |>
  dplyr::select(-case)
cell_te <-
  cells |>
  dplyr::filter(case == "Test") |>
  dplyr::select(-case, -class)

dense_plsda <-
  recipe(class ~ ., data = cell_tr) |>
  step_pls(all_numeric_predictors(), outcome = class, num_comp = 5)

sparse_plsda <-
  recipe(class ~ ., data = cell_tr) |>
  step_pls(all_numeric_predictors(), outcome = class, num_comp = 5,
           predictor_prop = 1 / 4)
```
