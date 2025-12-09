# PCA signal extraction

`step_pca()` creates a *specification* of a recipe step that will
convert numeric variables into one or more principal components.

## Usage

``` r
step_pca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 5,
  threshold = NA,
  options = list(),
  res = NULL,
  columns = NULL,
  prefix = "PC",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("pca")
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

- threshold:

  A fraction of the total variance that should be covered by the
  components. For example, `threshold = .75` means that `step_pca()`
  should generate enough components to capture 75 percent of the
  variability in the variables. Note: using this argument will override
  and reset any value given to `num_comp`.

- options:

  A list of options to the default method for
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html). Argument
  defaults are set to `retx = FALSE`, `center = FALSE`,
  `scale. = FALSE`, and `tol = NULL`. **Note** that the argument `x`
  should not be passed here (or at all).

- res:

  The [`stats::prcomp.default()`](https://rdrr.io/r/stats/prcomp.html)
  object is stored here once this preprocessing step has be trained by
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

Principal component analysis (PCA) is a transformation of a group of
variables that produces a new set of artificial features or components.
These components are designed to capture the maximum amount of
information (i.e. variance) in the original variables. Also, the
components are statistically independent from one another. This means
that they can be used to combat large inter-variables correlations in a
data set.

It is advisable to standardize the variables prior to running PCA. Here,
each variable will be centered and scaled prior to the PCA calculation.
This can be changed using the `options` argument or by using
[`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md)
and
[`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md).

The argument `num_comp` controls the number of components that will be
retained (the original variables that are used to derive the components
are removed from the data). The new components will have names that
begin with `prefix` and a sequence of numbers. The variable names are
padded with zeros. For example, if `num_comp < 10`, their names will be
`PC1` - `PC9`. If `num_comp = 101`, the names would be `PC1` - `PC101`.

Alternatively, `threshold` can be used to determine the number of
components that are required to capture a specified fraction of the
total variance in the variables.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step two things can happen depending the `type` argument. If
`type = "coef"` a tibble returned with 4 columns `terms`, `value`,
`component` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, variable loading

- component:

  character, principle component

- id:

  character, id of this step

If `type = "variance"` a tibble returned with 4 columns `terms`,
`value`, `component` , and `id`:

- terms:

  character, type of variance

- value:

  numeric, value of the variance

- component:

  integer, principle component

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `num_comp`: \# Components (type: integer, default: 5)

- `threshold`: Threshold (type: double, default: NA)

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## References

Jolliffe, I. T. (2010). *Principal Component Analysis*. Springer.

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
[`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
rec <- recipe(~., data = USArrests)
pca_trans <- rec |>
  step_normalize(all_numeric()) |>
  step_pca(all_numeric(), num_comp = 3)
pca_estimates <- prep(pca_trans, training = USArrests)
pca_data <- bake(pca_estimates, USArrests)

rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
plot(pca_data$PC1, pca_data$PC2,
  xlim = rng, ylim = rng
)


with_thresh <- rec |>
  step_normalize(all_numeric()) |>
  step_pca(all_numeric(), threshold = .99)
with_thresh <- prep(with_thresh, training = USArrests)
bake(with_thresh, USArrests)
#> # A tibble: 50 × 4
#>        PC1     PC2     PC3      PC4
#>      <dbl>   <dbl>   <dbl>    <dbl>
#>  1 -0.976  -1.12    0.440   0.155  
#>  2 -1.93   -1.06   -2.02   -0.434  
#>  3 -1.75    0.738  -0.0542 -0.826  
#>  4  0.140  -1.11   -0.113  -0.181  
#>  5 -2.50    1.53   -0.593  -0.339  
#>  6 -1.50    0.978  -1.08    0.00145
#>  7  1.34    1.08    0.637  -0.117  
#>  8 -0.0472  0.322   0.711  -0.873  
#>  9 -2.98   -0.0388  0.571  -0.0953 
#> 10 -1.62   -1.27    0.339   1.07   
#> # ℹ 40 more rows

tidy(pca_trans, number = 2)
#> # A tibble: 1 × 4
#>   terms         value component id       
#>   <chr>         <dbl> <chr>     <chr>    
#> 1 all_numeric()    NA NA        pca_d0Uw3
tidy(pca_estimates, number = 2)
#> # A tibble: 16 × 4
#>    terms      value component id       
#>    <chr>      <dbl> <chr>     <chr>    
#>  1 Murder   -0.536  PC1       pca_d0Uw3
#>  2 Assault  -0.583  PC1       pca_d0Uw3
#>  3 UrbanPop -0.278  PC1       pca_d0Uw3
#>  4 Rape     -0.543  PC1       pca_d0Uw3
#>  5 Murder   -0.418  PC2       pca_d0Uw3
#>  6 Assault  -0.188  PC2       pca_d0Uw3
#>  7 UrbanPop  0.873  PC2       pca_d0Uw3
#>  8 Rape      0.167  PC2       pca_d0Uw3
#>  9 Murder    0.341  PC3       pca_d0Uw3
#> 10 Assault   0.268  PC3       pca_d0Uw3
#> 11 UrbanPop  0.378  PC3       pca_d0Uw3
#> 12 Rape     -0.818  PC3       pca_d0Uw3
#> 13 Murder    0.649  PC4       pca_d0Uw3
#> 14 Assault  -0.743  PC4       pca_d0Uw3
#> 15 UrbanPop  0.134  PC4       pca_d0Uw3
#> 16 Rape      0.0890 PC4       pca_d0Uw3
tidy(pca_estimates, number = 2, type = "variance")
#> # A tibble: 16 × 4
#>    terms                         value component id       
#>    <chr>                         <dbl>     <int> <chr>    
#>  1 variance                      2.48          1 pca_d0Uw3
#>  2 variance                      0.990         2 pca_d0Uw3
#>  3 variance                      0.357         3 pca_d0Uw3
#>  4 variance                      0.173         4 pca_d0Uw3
#>  5 cumulative variance           2.48          1 pca_d0Uw3
#>  6 cumulative variance           3.47          2 pca_d0Uw3
#>  7 cumulative variance           3.83          3 pca_d0Uw3
#>  8 cumulative variance           4             4 pca_d0Uw3
#>  9 percent variance             62.0           1 pca_d0Uw3
#> 10 percent variance             24.7           2 pca_d0Uw3
#> 11 percent variance              8.91          3 pca_d0Uw3
#> 12 percent variance              4.34          4 pca_d0Uw3
#> 13 cumulative percent variance  62.0           1 pca_d0Uw3
#> 14 cumulative percent variance  86.8           2 pca_d0Uw3
#> 15 cumulative percent variance  95.7           3 pca_d0Uw3
#> 16 cumulative percent variance 100             4 pca_d0Uw3
```
