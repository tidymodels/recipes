# Isomap embedding

`step_isomap()` creates a *specification* of a recipe step that uses
multidimensional scaling to convert numeric data into one or more new
dimensions.

## Usage

``` r
step_isomap(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_terms = 5,
  neighbors = 50,
  options = list(.mute = c("message", "output")),
  res = NULL,
  columns = NULL,
  prefix = "Isomap",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("isomap")
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

- num_terms:

  The number of isomap dimensions to retain as new predictors. If
  `num_terms` is greater than the number of columns or the number of
  possible dimensions, a smaller value will be used.

- neighbors:

  The number of neighbors.

- options:

  A list of options to \`dimRed::embed()“.

- res:

  The \`dimRed::embed()“ object is stored here once this preprocessing
  step has be trained by
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

Isomap is a form of multidimensional scaling (MDS). MDS methods try to
find a reduced set of dimensions such that the geometric distances
between the original data points are preserved. This version of MDS uses
nearest neighbors in the data as a method for increasing the fidelity of
the new dimensions to the original data values.

This step requires the dimRed, RSpectra, igraph, and RANN packages. If
not installed, the step will stop with a note about installing these
packages.

It is advisable to center and scale the variables prior to running
Isomap (`step_center` and `step_scale` can be used for this purpose).

The argument `num_terms` controls the number of components that will be
retained (the original variables that are used to derive the components
are removed from the data). The new components will have names that
begin with `prefix` and a sequence of numbers. The variable names are
padded with zeros. For example, if `num_terms < 10`, their names will be
`Isomap1` - `Isomap9`. If `num_terms = 101`, the names would be
`Isomap001` - `Isomap101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` , and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `num_terms`: \# Model Terms (type: integer, default: 5)

- `neighbors`: \# Nearest Neighbors (type: integer, default: 50)

## Case weights

The underlying operation does not allow for case weights.

## References

De Silva, V., and Tenenbaum, J. B. (2003). Global versus local methods
in nonlinear dimensionality reduction. *Advances in Neural Information
Processing Systems*. 721-728.

dimRed, a framework for dimensionality reduction,
https://github.com/gdkrmr

## See also

Other multivariate transformation steps:
[`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
[`step_classdist_shrunken()`](https://recipes.tidymodels.org/dev/reference/step_classdist_shrunken.md),
[`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
[`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md),
[`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md),
[`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md),
[`step_kpca_poly()`](https://recipes.tidymodels.org/dev/reference/step_kpca_poly.md),
[`step_kpca_rbf()`](https://recipes.tidymodels.org/dev/reference/step_kpca_rbf.md),
[`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
[`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md),
[`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
[`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
if (FALSE) {
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

im_trans <- rec |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_isomap(all_numeric_predictors(), neighbors = 100, num_terms = 2)

im_estimates <- prep(im_trans, training = biomass_tr)

im_te <- bake(im_estimates, biomass_te)

rng <- extendrange(c(im_te$Isomap1, im_te$Isomap2))
plot(im_te$Isomap1, im_te$Isomap2,
  xlim = rng, ylim = rng
)

tidy(im_trans, number = 3)
tidy(im_estimates, number = 3)
}
```
