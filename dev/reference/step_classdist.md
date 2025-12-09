# Distances to class centroids

`step_classdist()` creates a *specification* of a recipe step that will
convert numeric data into Mahalanobis distance measurements to the data
centroid. This is done for each value of a categorical class variable.

## Usage

``` r
step_classdist(
  recipe,
  ...,
  class,
  role = "predictor",
  trained = FALSE,
  mean_func = mean,
  cov_func = cov,
  pool = FALSE,
  log = TRUE,
  objects = NULL,
  prefix = "classdist_",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("classdist")
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

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- mean_func:

  A function to compute the center of the distribution.

- cov_func:

  A function that computes the covariance matrix

- pool:

  A logical: should the covariance matrix be computed by pooling the
  data for all of the classes?

- log:

  A logical: should the distances be transformed by the natural log
  function?

- objects:

  Statistics are stored here once this step has been trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- prefix:

  A character string for the prefix of the resulting new variables. See
  notes below.

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

## Details

`step_classdist()` will create a new column for every unique value of
the `class` variable. The resulting variables will not replace the
original values and, by default, have the prefix `classdist_`. The
naming format can be changed using the `prefix` argument.

Class-specific centroids are the multivariate averages of each predictor
using the data from each class in the training set. When pre-processing
a new data point, this step computes the distance from the new point to
each of the class centroids. These distance features can be very
effective at capturing linear class boundaries. For this reason, they
can be useful to add to an existing predictor set used within a
nonlinear model. If the true boundary is actually linear, the model will
have an easier time learning the training data patterns.

Note that, by default, the default covariance function requires that
each class should have at least as many rows as variables listed in the
`terms` argument. If `pool = TRUE`, there must be at least as many data
points are variables overall.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`, `class` ,
and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, location of centroid

- class:

  character, name of the class

- id:

  character, id of this step

## Case weights

This step performs an supervised operation that can utilize case
weights. As a result, case weights are used with frequency weights as
well as importance weights. For more information,, see the documentation
in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other multivariate transformation steps:
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
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
[`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

## Examples

``` r
data(penguins, package = "modeldata")
penguins <- penguins[vctrs::vec_detect_complete(penguins), ]
penguins$island <- NULL
penguins$sex <- NULL

# in case of missing data...
mean2 <- function(x) mean(x, na.rm = TRUE)

# define naming convention
rec <- recipe(species ~ ., data = penguins) |>
  step_classdist(all_numeric_predictors(),
    class = species,
    pool = FALSE, mean_func = mean2, prefix = "centroid_"
  )

# default naming
rec <- recipe(species ~ ., data = penguins) |>
  step_classdist(all_numeric_predictors(),
    class = species,
    pool = FALSE, mean_func = mean2
  )

rec_dists <- prep(rec, training = penguins)

dists_to_species <- bake(rec_dists, new_data = penguins)
## on log scale:
dist_cols <- grep("classdist", names(dists_to_species), value = TRUE)
dists_to_species[, c("species", dist_cols)]
#> # A tibble: 333 × 4
#>    species classdist_Adelie classdist_Chinstrap classdist_Gentoo
#>    <fct>              <dbl>               <dbl>            <dbl>
#>  1 Adelie             1.04                 3.19             5.10
#>  2 Adelie             0.670                2.61             4.61
#>  3 Adelie             1.45                 2.39             4.68
#>  4 Adelie             1.20                 3.42             5.08
#>  5 Adelie             1.72                 3.57             5.35
#>  6 Adelie             0.903                2.87             4.90
#>  7 Adelie             1.78                 3.61             4.89
#>  8 Adelie             1.66                 2.17             4.88
#>  9 Adelie             2.05                 3.85             5.45
#> 10 Adelie             2.72                 4.24             5.35
#> # ℹ 323 more rows

tidy(rec, number = 1)
#> # A tibble: 1 × 4
#>   terms                    value class id             
#>   <chr>                    <dbl> <chr> <chr>          
#> 1 all_numeric_predictors()    NA NA    classdist_Gpw7o
tidy(rec_dists, number = 1)
#> # A tibble: 12 × 4
#>    terms              value class     id             
#>    <chr>              <dbl> <chr>     <chr>          
#>  1 bill_length_mm      38.8 Adelie    classdist_Gpw7o
#>  2 bill_depth_mm       18.3 Adelie    classdist_Gpw7o
#>  3 flipper_length_mm  190.  Adelie    classdist_Gpw7o
#>  4 body_mass_g       3706.  Adelie    classdist_Gpw7o
#>  5 bill_length_mm      48.8 Chinstrap classdist_Gpw7o
#>  6 bill_depth_mm       18.4 Chinstrap classdist_Gpw7o
#>  7 flipper_length_mm  196.  Chinstrap classdist_Gpw7o
#>  8 body_mass_g       3733.  Chinstrap classdist_Gpw7o
#>  9 bill_length_mm      47.6 Gentoo    classdist_Gpw7o
#> 10 bill_depth_mm       15.0 Gentoo    classdist_Gpw7o
#> 11 flipper_length_mm  217.  Gentoo    classdist_Gpw7o
#> 12 body_mass_g       5092.  Gentoo    classdist_Gpw7o
```
