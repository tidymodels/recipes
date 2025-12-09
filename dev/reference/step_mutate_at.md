# Mutate multiple columns using dplyr

**\[superseded\]**

`step_mutate_at()` is superseded in favor of using
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md)
with
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html).

`step_mutate_at()` creates a *specification* of a recipe step that will
modify the selected variables using a common function via
[`dplyr::mutate_at()`](https://dplyr.tidyverse.org/reference/mutate_all.html).

## Usage

``` r
step_mutate_at(
  recipe,
  ...,
  fn,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate_at")
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

- fn:

  A function fun, a quosure style lambda `~ fun(.)` or a list of either
  form. (see
  [`dplyr::mutate_at()`](https://dplyr.tidyverse.org/reference/mutate_all.html)).
  **Note that this argument must be named**.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- inputs:

  A vector of column names populated by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

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

When using this flexible step, use extra care to avoid data leakage in
your preprocessing. Consider, for example, the transformation
`x = w > mean(w)`. When applied to new data or testing data, this
transformation would use the mean of `w` from the *new* data, not the
mean of `w` from the training data.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

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
[`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md),
[`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
[`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

Other dplyr steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md),
[`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
library(dplyr)
recipe(~., data = iris) |>
  step_mutate_at(contains("Length"), fn = ~ 1 / .) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:10)
#> # A tibble: 10 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1        0.196         3.5        0.714         0.2 setosa 
#>  2        0.204         3          0.714         0.2 setosa 
#>  3        0.213         3.2        0.769         0.2 setosa 
#>  4        0.217         3.1        0.667         0.2 setosa 
#>  5        0.2           3.6        0.714         0.2 setosa 
#>  6        0.185         3.9        0.588         0.4 setosa 
#>  7        0.217         3.4        0.714         0.3 setosa 
#>  8        0.2           3.4        0.667         0.2 setosa 
#>  9        0.227         2.9        0.714         0.2 setosa 
#> 10        0.204         3.1        0.667         0.1 setosa 

recipe(~., data = iris) |>
  # leads to more columns being created.
  step_mutate_at(contains("Length"), fn = list(log = log, sqrt = sqrt)) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:10)
#> # A tibble: 10 × 9
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 4 more variables: Sepal.Length_log <dbl>, Petal.Length_log <dbl>,
#> #   Sepal.Length_sqrt <dbl>, Petal.Length_sqrt <dbl>
```
