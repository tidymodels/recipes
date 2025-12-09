# Distance between two locations

`step_geodist()` creates a *specification* of a recipe step that will
calculate the distance between points on a map to a reference location.

## Usage

``` r
step_geodist(
  recipe,
  lat = NULL,
  lon = NULL,
  role = "predictor",
  trained = FALSE,
  ref_lat = NULL,
  ref_lon = NULL,
  is_lat_lon = TRUE,
  log = FALSE,
  name = "geo_dist",
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("geodist")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- lon, lat:

  Selector functions to choose which variables are used by the step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- ref_lon, ref_lat:

  Single numeric values for the location of the reference point.

- is_lat_lon:

  A logical: Are coordinates in latitude and longitude? If `TRUE` the
  Haversine formula is used and the returned result is meters. If
  `FALSE` the Pythagorean formula is used. Default is `TRUE` and for
  recipes created from previous versions of recipes, a value of `FALSE`
  is used.

- log:

  A logical: should the distance be transformed by the natural log
  function?

- name:

  A single character value to use for the new predictor column. If a
  column exists with this name, an error is issued.

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

## Details

`step_geodist` uses the Pythagorean theorem to calculate Euclidean
distances if `is_lat_lon` is FALSE. If `is_lat_lon` is TRUE, the
Haversine formula is used to calculate the great-circle distance in
meters.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `latitude`, `longitude`,
`ref_latitude`, `ref_longitude`, `is_lat_lon`, `name` , and `id`:

- latitude:

  character, name of latitude variable

- longitude:

  character, name of longitude variable

- ref_latitude:

  numeric, location of latitude reference point

- ref_longitude:

  numeric, location of longitude reference point

- is_lat_lon:

  character, the summary function name

- name:

  character, name of resulting variable

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## References

https://en.wikipedia.org/wiki/Haversine_formula

## See also

Other multivariate transformation steps:
[`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
[`step_classdist_shrunken()`](https://recipes.tidymodels.org/dev/reference/step_classdist_shrunken.md),
[`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
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
data(Smithsonian, package = "modeldata")

# How close are the museums to Union Station?
near_station <- recipe(~., data = Smithsonian) |>
  update_role(name, new_role = "location") |>
  step_geodist(
    lat = latitude, lon = longitude, log = FALSE,
    ref_lat = 38.8986312, ref_lon = -77.0062457,
    is_lat_lon = TRUE
  ) |>
  prep(training = Smithsonian)

bake(near_station, new_data = NULL) |>
  arrange(geo_dist)
#> # A tibble: 20 × 4
#>    name                                     latitude longitude geo_dist
#>    <chr>                                       <dbl>     <dbl>    <dbl>
#>  1 National Postal Museum                       38.9     -77.0     367.
#>  2 Renwick Gallery                              38.9     -77.0     932.
#>  3 National Museum of the American Indian       38.9     -77.0    1571.
#>  4 Smithsonian American Art Museum              38.9     -77.0    1636.
#>  5 National Portrait Gallery                    38.9     -77.0    1646.
#>  6 National Air and Space Museum                38.9     -77.0    1796.
#>  7 Hirshhorn Museum and Sculpture Garden        38.9     -77.0    2008.
#>  8 National Museum of Natural History           38.9     -77.0    2073.
#>  9 Arthur M. Sackler Gallery                    38.9     -77.0    2108.
#> 10 Arts and Industries Building                 38.9     -77.0    2124.
#> 11 Smithsonian Institution Building             38.9     -77.0    2193.
#> 12 National Museum of African Art               38.9     -77.0    2202.
#> 13 Freer Gallery of Art                         38.9     -77.0    2266.
#> 14 National Museum of American History          38.9     -77.0    2393.
#> 15 National Museum of African American His…     38.9     -77.0    2611.
#> 16 National Zoological Park                     38.9     -77.1    5246.
#> 17 Anacostia Community Museum                   38.9     -77.0    5332.
#> 18 Steven F. Udvar-Hazy Center                  38.9     -77.4   38111.
#> 19 George Gustav Heye Center                    40.7     -74.0  324871.
#> 20 Cooper Hewitt, Smithsonian Design Museum     40.8     -74.0  334041.

tidy(near_station, number = 1)
#> # A tibble: 1 × 7
#>   latitude longitude ref_latitude ref_longitude is_lat_lon name   id   
#>   <chr>    <chr>            <dbl>         <dbl> <lgl>      <chr>  <chr>
#> 1 latitude longitude         38.9         -77.0 TRUE       geo_d… geod…
```
