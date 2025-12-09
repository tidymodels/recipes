# Compute shrunken centroid distances for classification models

`step_classdist_shrunken()` creates a *specification* of a recipe step
that will convert numeric data into Euclidean distance to the
regularized class centroid. This is done for each value of a categorical
class variable.

## Usage

``` r
step_classdist_shrunken(
  recipe,
  ...,
  class = NULL,
  role = NA,
  trained = FALSE,
  threshold = 1/2,
  sd_offset = 1/2,
  log = TRUE,
  prefix = "classdist_",
  keep_original_cols = TRUE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("classdist_shrunken")
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

- class:

  A bare name that specifies a single categorical variable to be used as
  the class. Can also be a string or tidyselect for backwards
  compatibility.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- threshold:

  A regularization parameter between zero and one. Zero means that no
  regularization is used and one means that centroids should be shrunk
  to the global centroid.

- sd_offset:

  A value between zero and one for the quantile that should be used to
  stabilize the pooled standard deviation.

- log:

  A logical: should the distances be transformed by the natural log
  function?

- prefix:

  A character string for the prefix of the resulting new variables. See
  notes below.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `TRUE`.

- objects:

  Statistics are stored here once this step has been trained by
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

## Details

Class-specific centroids are the multivariate averages of each predictor
using the data from each class in the training set. When pre-processing
a new data point, this step computes the distance from the new point to
each of the class centroids. These distance features can be very
effective at capturing linear class boundaries. For this reason, they
can be useful to add to an existing predictor set used within a
nonlinear model. If the true boundary is actually linear, the model will
have an easier time learning the training data patterns.

Shrunken centroids use a form of regularization where the class-specific
centroids are contracted to the overall class-independent centroid. If a
predictor is uninformative, shrinking it may move it entirely to the
overall centroid. This has the effect of removing that predictor's
effect on the new distance features. However, it may not move all of the
class-specific features to the center in many cases. This means that
some features will only affect the classification of specific classes.

The `threshold` parameter can be used to optimized how much
regularization should be used.

`step_classdist_shrunken()` will create a new column for every unique
value of the `class` variable. The resulting variables will not replace
the original values and, by default, have the prefix `classdist_`. The
naming format can be changed using the `prefix` argument.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`, `class`,
`type`, `threshold` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the centroid

- class:

  character, name of class variable

- type:

  character, has values `"global"`, `"by_class"`, and `"shrunken"`

- threshold:

  numeric, value of threshold

- id:

  character, id of this step

The first two types of centroids are in the original units while the
last has been standardized.

## Case weights

This step performs an supervised operation that can utilize case
weights. As a result, case weights are used with frequency weights as
well as importance weights. For more information,, see the documentation
in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## References

Tibshirani, R., Hastie, T., Narasimhan, B., & Chu, G. (2002). Diagnosis
of multiple cancer types by shrunken centroids of gene expression.
*Proceedings of the National Academy of Sciences*, 99(10), 6567-6572.

## See also

Other multivariate transformation steps:
[`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
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
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
data(penguins, package = "modeldata")
penguins <- penguins[vctrs::vec_detect_complete(penguins), ]
penguins$island <- NULL
penguins$sex <- NULL

# define naming convention
rec <- recipe(species ~ ., data = penguins) |>
  step_classdist_shrunken(all_numeric_predictors(),
    class = species,
    threshold = 1 / 4, prefix = "centroid_"
  )

# default naming
rec <- recipe(species ~ ., data = penguins) |>
  step_classdist_shrunken(all_numeric_predictors(),
    class = species,
    threshold = 3 / 4
  )

rec_dists <- prep(rec, training = penguins)

dists_to_species <- bake(rec_dists, new_data = penguins)
## on log scale:
dist_cols <- grep("classdist", names(dists_to_species), value = TRUE)
dists_to_species[, c("species", dist_cols)]
#> # A tibble: 333 × 4
#>    species classdist_Adelie classdist_Gentoo classdist_Chinstrap
#>    <fct>              <dbl>            <dbl>               <dbl>
#>  1 Adelie             1.49            1.72                 1.49 
#>  2 Adelie             1.03            1.35                 1.03 
#>  3 Adelie             1.56            1.93                 1.56 
#>  4 Adelie             1.42            1.78                 1.42 
#>  5 Adelie             1.11            1.48                 1.11 
#>  6 Adelie             1.61            1.86                 1.61 
#>  7 Adelie             0.602           0.0916               0.602
#>  8 Adelie             2.02            2.29                 2.02 
#>  9 Adelie             0.898           1.26                 0.898
#> 10 Adelie             0.756           0.673                0.756
#> # ℹ 323 more rows

tidy(rec, number = 1)
#> # A tibble: 1 × 6
#>   terms                    value class type  threshold id              
#>   <chr>                    <dbl> <chr> <chr>     <dbl> <chr>           
#> 1 all_numeric_predictors()    NA NA    NA           NA classdist_shrun…
tidy(rec_dists, number = 1)
#> # A tibble: 36 × 6
#>    terms          value class     type     threshold id                
#>    <chr>          <dbl> <chr>     <chr>        <dbl> <chr>             
#>  1 bill_length_mm  44.0 Adelie    global        0.75 classdist_shrunke…
#>  2 bill_length_mm  38.8 Adelie    by_class      0.75 classdist_shrunke…
#>  3 bill_length_mm   0   Adelie    shrunken      0.75 classdist_shrunke…
#>  4 bill_length_mm  44.0 Gentoo    global        0.75 classdist_shrunke…
#>  5 bill_length_mm  47.6 Gentoo    by_class      0.75 classdist_shrunke…
#>  6 bill_length_mm   0   Gentoo    shrunken      0.75 classdist_shrunke…
#>  7 bill_length_mm  44.0 Chinstrap global        0.75 classdist_shrunke…
#>  8 bill_length_mm  48.8 Chinstrap by_class      0.75 classdist_shrunke…
#>  9 bill_length_mm   0   Chinstrap shrunken      0.75 classdist_shrunke…
#> 10 bill_depth_mm   17.2 Adelie    global        0.75 classdist_shrunke…
#> # ℹ 26 more rows
```
