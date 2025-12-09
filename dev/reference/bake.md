# Apply a trained preprocessing recipe

For a recipe with at least one preprocessing operation that has been
trained by
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), apply
the computations to new data.

## Usage

``` r
bake(object, ...)

# S3 method for class 'recipe'
bake(object, new_data, ..., composition = "tibble")
```

## Arguments

- object:

  A trained object such as a
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  with at least one preprocessing operation.

- ...:

  One or more selector functions to choose which variables will be
  returned by the function. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details. If no selectors are given, the default is to use
  [`dplyr::everything()`](https://dplyr.tidyverse.org/reference/reexports.html).

- new_data:

  A data frame, tibble, or sparse matrix from the `Matrix` package for
  whom the preprocessing will be applied. If `NULL` is given to
  `new_data`, the pre-processed *training data* will be returned
  (assuming that `prep(retain = TRUE)` was used). See
  [sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
  for more information about use of sparse data.

- composition:

  Either `"tibble"`, `"matrix"`, `"data.frame"`, or
  ``` "dgCMatrix"``for the format of the processed data set. Also, note that this argument should be called **after** any selectors and the selectors should only resolve to numeric columns if  ```composition`is set to`"matrix"`or`"dgCMatrix"`. If the data contains sparse columns they will be perseved for `"tibble"`and`"data.frame"`, and efficiently used for `"dgCMatrix"\`.

## Value

A tibble, matrix, or sparse matrix that may have different columns than
the original columns in `new_data`.

## Details

`bake()` takes a trained recipe and applies its operations to a data set
to create a design matrix. If you are using a recipe as a preprocessor
for modeling, we **highly recommend** that you use a `workflow()`
instead of manually applying a recipe (see the example in
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)).

If the data set is not too large, time can be saved by using the
`retain = TRUE` option of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md). This
stores the processed version of the training set. With this option set,
`bake(object, new_data = NULL)` will return it for free.

Also, any steps with `skip = TRUE` will not be applied to the data when
`bake()` is invoked with a data set in `new_data`.
`bake(object, new_data = NULL)` will always have all of the steps
applied.

## See also

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md) and
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)

## Examples

``` r
data(ames, package = "modeldata")

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

ames_rec <-
  recipe(Sale_Price ~ ., data = ames[-(1:6), ]) |>
  step_other(Neighborhood, threshold = 0.05) |>
  step_dummy(all_nominal()) |>
  step_interact(~ starts_with("Central_Air"):Year_Built) |>
  step_ns(Longitude, Latitude, deg_free = 2) |>
  step_zv(all_predictors()) |>
  prep()

# return the training set (already embedded in ames_rec)
bake(ames_rec, new_data = NULL)
#> # A tibble: 2,924 × 259
#>    Lot_Frontage Lot_Area Year_Built Year_Remod_Add Mas_Vnr_Area
#>           <dbl>    <int>      <int>          <int>        <dbl>
#>  1           41     4920       2001           2001            0
#>  2           43     5005       1992           1992            0
#>  3           39     5389       1995           1996            0
#>  4           60     7500       1999           1999            0
#>  5           75    10000       1993           1994            0
#>  6            0     7980       1992           2007            0
#>  7           63     8402       1998           1998            0
#>  8           85    10176       1990           1990            0
#>  9            0     6820       1985           1985            0
#> 10           47    53504       2003           2003          603
#> # ℹ 2,914 more rows
#> # ℹ 254 more variables: BsmtFin_SF_1 <dbl>, BsmtFin_SF_2 <dbl>,
#> #   Bsmt_Unf_SF <dbl>, Total_Bsmt_SF <dbl>, First_Flr_SF <int>,
#> #   Second_Flr_SF <int>, Gr_Liv_Area <int>, Bsmt_Full_Bath <dbl>,
#> #   Bsmt_Half_Bath <dbl>, Full_Bath <int>, Half_Bath <int>,
#> #   Bedroom_AbvGr <int>, Kitchen_AbvGr <int>, TotRms_AbvGrd <int>,
#> #   Fireplaces <int>, Garage_Cars <dbl>, Garage_Area <dbl>, …

# apply processing to other data:
bake(ames_rec, new_data = head(ames))
#> # A tibble: 6 × 259
#>   Lot_Frontage Lot_Area Year_Built Year_Remod_Add Mas_Vnr_Area
#>          <dbl>    <int>      <int>          <int>        <dbl>
#> 1          141    31770       1960           1960          112
#> 2           80    11622       1961           1961            0
#> 3           81    14267       1958           1958          108
#> 4           93    11160       1968           1968            0
#> 5           74    13830       1997           1998            0
#> 6           78     9978       1998           1998           20
#> # ℹ 254 more variables: BsmtFin_SF_1 <dbl>, BsmtFin_SF_2 <dbl>,
#> #   Bsmt_Unf_SF <dbl>, Total_Bsmt_SF <dbl>, First_Flr_SF <int>,
#> #   Second_Flr_SF <int>, Gr_Liv_Area <int>, Bsmt_Full_Bath <dbl>,
#> #   Bsmt_Half_Bath <dbl>, Full_Bath <int>, Half_Bath <int>,
#> #   Bedroom_AbvGr <int>, Kitchen_AbvGr <int>, TotRms_AbvGrd <int>,
#> #   Fireplaces <int>, Garage_Cars <dbl>, Garage_Area <dbl>,
#> #   Wood_Deck_SF <int>, Open_Porch_SF <int>, Enclosed_Porch <int>, …

# only return selected variables:
bake(ames_rec, new_data = head(ames), all_numeric_predictors())
#> # A tibble: 6 × 258
#>   Lot_Frontage Lot_Area Year_Built Year_Remod_Add Mas_Vnr_Area
#>          <dbl>    <int>      <int>          <int>        <dbl>
#> 1          141    31770       1960           1960          112
#> 2           80    11622       1961           1961            0
#> 3           81    14267       1958           1958          108
#> 4           93    11160       1968           1968            0
#> 5           74    13830       1997           1998            0
#> 6           78     9978       1998           1998           20
#> # ℹ 253 more variables: BsmtFin_SF_1 <dbl>, BsmtFin_SF_2 <dbl>,
#> #   Bsmt_Unf_SF <dbl>, Total_Bsmt_SF <dbl>, First_Flr_SF <int>,
#> #   Second_Flr_SF <int>, Gr_Liv_Area <int>, Bsmt_Full_Bath <dbl>,
#> #   Bsmt_Half_Bath <dbl>, Full_Bath <int>, Half_Bath <int>,
#> #   Bedroom_AbvGr <int>, Kitchen_AbvGr <int>, TotRms_AbvGrd <int>,
#> #   Fireplaces <int>, Garage_Cars <dbl>, Garage_Area <dbl>,
#> #   Wood_Deck_SF <int>, Open_Porch_SF <int>, Enclosed_Porch <int>, …
bake(ames_rec, new_data = head(ames), starts_with(c("Longitude", "Latitude")))
#> # A tibble: 6 × 4
#>   Longitude_ns_1 Longitude_ns_2 Latitude_ns_1 Latitude_ns_2
#>            <dbl>          <dbl>         <dbl>         <dbl>
#> 1          0.570       -0.0141          0.472         0.394
#> 2          0.570       -0.0142          0.481         0.360
#> 3          0.569       -0.00893         0.484         0.348
#> 4          0.563        0.0212          0.496         0.301
#> 5          0.562       -0.212           0.405         0.634
#> 6          0.562       -0.212           0.407         0.630
```
