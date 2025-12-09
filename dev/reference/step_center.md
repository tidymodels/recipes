# Centering numeric data

`step_center()` creates a *specification* of a recipe step that will
normalize numeric data to have a mean of zero.

## Usage

``` r
step_center(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  means = NULL,
  na_rm = TRUE,
  skip = FALSE,
  id = rand_id("center")
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

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- means:

  A named numeric vector of means. This is `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- na_rm:

  A logical value indicating whether `NA` values should be removed
  during computations.

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

Centering data means that the average of a variable is subtracted from
the data. `step_center()` estimates the variable means from the data
used in the `training` argument of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) then
applies the centering to new data sets using these means.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the means

- id:

  character, id of this step

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other normalization steps:
[`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md),
[`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md),
[`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

center_trans <- rec |>
  step_center(carbon, contains("gen"), -hydrogen)

center_obj <- prep(center_trans, training = biomass_tr)

transformed_te <- bake(center_obj, biomass_te)

biomass_te[1:10, names(transformed_te)]
#>    carbon hydrogen oxygen nitrogen sulfur    HHV
#> 15  46.35     5.67  47.20     0.30   0.22 18.275
#> 20  43.25     5.50  48.06     2.85   0.34 17.560
#> 26  42.70     5.50  49.10     2.40   0.30 17.173
#> 31  46.40     6.10  37.30     1.80   0.50 18.851
#> 36  48.76     6.32  42.77     0.20   0.00 20.547
#> 41  44.30     5.50  41.70     0.70   0.20 18.467
#> 46  38.94     5.23  54.13     1.19   0.51 15.095
#> 51  42.10     4.66  33.80     0.95   0.20 16.240
#> 55  29.20     4.40  31.10     0.14   4.90 11.147
#> 65  27.80     3.77  23.69     4.63   1.05 10.750
transformed_te
#> # A tibble: 80 × 6
#>     carbon hydrogen oxygen nitrogen sulfur   HHV
#>      <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>
#>  1  -2.00      5.67   8.68   -0.775   0.22  18.3
#>  2  -5.10      5.5    9.54    1.78    0.34  17.6
#>  3  -5.65      5.5   10.6     1.33    0.3   17.2
#>  4  -1.95      6.1   -1.22    0.725   0.5   18.9
#>  5   0.406     6.32   4.25   -0.875   0     20.5
#>  6  -4.05      5.5    3.18   -0.375   0.2   18.5
#>  7  -9.41      5.23  15.6     0.115   0.51  15.1
#>  8  -6.25      4.66  -4.72   -0.125   0.2   16.2
#>  9 -19.2       4.4   -7.42   -0.935   4.9   11.1
#> 10 -20.6       3.77 -14.8     3.56    1.05  10.8
#> # ℹ 70 more rows

tidy(center_trans, number = 1)
#> # A tibble: 3 × 3
#>   terms               value id          
#>   <chr>               <dbl> <chr>       
#> 1 "carbon"               NA center_bgrz7
#> 2 "contains(\"gen\")"    NA center_bgrz7
#> 3 "-hydrogen"            NA center_bgrz7
tidy(center_obj, number = 1)
#> # A tibble: 3 × 3
#>   terms    value id          
#>   <chr>    <dbl> <chr>       
#> 1 carbon   48.4  center_bgrz7
#> 2 oxygen   38.5  center_bgrz7
#> 3 nitrogen  1.07 center_bgrz7
```
