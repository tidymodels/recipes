# Data depths

`step_depth()` creates a *specification* of a recipe step that will
convert numeric data into a measurement of *data depth*. This is done
for each value of a categorical class variable.

## Usage

``` r
step_depth(
  recipe,
  ...,
  class,
  role = "predictor",
  trained = FALSE,
  metric = "halfspace",
  options = list(),
  data = NULL,
  prefix = "depth_",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("depth")
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

- metric:

  A character string specifying the depth metric. Possible values are
  `"potential"`, `"halfspace"`, `"Mahalanobis"`, `"simplicialVolume"`,
  `"spatial"`, and `"zonoid"`.

- options:

  A list of options to pass to the underlying depth functions. See
  [`ddalpha::depth.halfspace()`](https://rdrr.io/pkg/ddalpha/man/depth.halfspace.html),
  [`ddalpha::depth.Mahalanobis()`](https://rdrr.io/pkg/ddalpha/man/depth.Mahalanobis.html),
  [`ddalpha::depth.potential()`](https://rdrr.io/pkg/ddalpha/man/depth.potential.html),
  [`ddalpha::depth.projection()`](https://rdrr.io/pkg/ddalpha/man/depth.projection.html),
  [`ddalpha::depth.simplicial()`](https://rdrr.io/pkg/ddalpha/man/depth.simplicial.html),
  [`ddalpha::depth.simplicialVolume()`](https://rdrr.io/pkg/ddalpha/man/depth.simplicialVolume.html),
  [`ddalpha::depth.spatial()`](https://rdrr.io/pkg/ddalpha/man/depth.spatial.html),
  [`ddalpha::depth.zonoid()`](https://rdrr.io/pkg/ddalpha/man/depth.zonoid.html).

- data:

  The training data are stored here once after
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  executed.

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

Data depth metrics attempt to measure how close data a data point is to
the center of its distribution. There are a number of methods for
calculating depth but a simple example is the inverse of the distance of
a data point to the centroid of the distribution. Generally, small
values indicate that a data point not close to the centroid.
`step_depth()` can compute a class-specific depth for a new data point
based on the proximity of the new value to the training set
distribution.

This step requires the ddalpha package. If not installed, the step will
stop with a note about installing the package.

Note that the entire training set is saved to compute future depth
values. The saved data have been trained (i.e. prepared) and baked (i.e.
processed) up to the point before the location that `step_depth()`
occupies in the recipe. Also, the data requirements for the different
step methods may vary. For example, using `metric = "Mahalanobis"`
requires that each class should have at least as many rows as variables
listed in the `terms` argument.

The function will create a new column for every unique value of the
`class` variable. The resulting variables will not replace the original
values and by default have the prefix `depth_`. The naming format can be
changed using the `prefix` argument.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `class` , and
`id`:

- terms:

  character, the selectors or variables selected

- class:

  character, name of class variable

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other multivariate transformation steps:
[`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
[`step_classdist_shrunken()`](https://recipes.tidymodels.org/dev/reference/step_classdist_shrunken.md),
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
# halfspace depth is the default
rec <- recipe(Species ~ ., data = iris) |>
  step_depth(all_numeric_predictors(), class = Species)

# use zonoid metric instead
# also, define naming convention for new columns
rec <- recipe(Species ~ ., data = iris) |>
  step_depth(all_numeric_predictors(),
    class = Species,
    metric = "zonoid", prefix = "zonoid_"
  )

rec_dists <- prep(rec, training = iris)

dists_to_species <- bake(rec_dists, new_data = iris)
dists_to_species
#> # A tibble: 150 × 8
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
#> # ℹ 140 more rows
#> # ℹ 3 more variables: zonoid_setosa <dbl>, zonoid_versicolor <dbl>,
#> #   zonoid_virginica <dbl>

tidy(rec, number = 1)
#> # A tibble: 1 × 3
#>   terms                    class id         
#>   <chr>                    <chr> <chr>      
#> 1 all_numeric_predictors() NA    depth_hRypf
tidy(rec_dists, number = 1)
#> # A tibble: 4 × 3
#>   terms        class   id         
#>   <chr>        <chr>   <chr>      
#> 1 Sepal.Length Species depth_hRypf
#> 2 Sepal.Width  Species depth_hRypf
#> 3 Petal.Length Species depth_hRypf
#> 4 Petal.Width  Species depth_hRypf
```
