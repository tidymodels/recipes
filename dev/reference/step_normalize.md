# Center and scale numeric data

`step_normalize()` creates a *specification* of a recipe step that will
normalize numeric data to have a standard deviation of one and a mean of
zero.

## Usage

``` r
step_normalize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  means = NULL,
  sds = NULL,
  na_rm = TRUE,
  skip = FALSE,
  id = rand_id("normalize")
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

- sds:

  A named numeric vector of standard deviations This is `NULL` until
  computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- na_rm:

  A logical value indicating whether `NA` values should be removed when
  computing the standard deviation and mean.

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
the data. Scaling data means that the standard deviation of a variable
is divided out of the data. `step_normalize()` estimates the variable
standard deviations and means from the data used in the `training`
argument of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) then
applies the scaling to new data sets using these estimates.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `statistic`,
`value` , and `id`:

- terms:

  character, the selectors or variables selected

- statistic:

  character, name of statistic (`"mean"` or `"sd"`)

- value:

  numeric, value of the `statistic`

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
[`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md),
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

norm_trans <- rec |>
  step_normalize(carbon, hydrogen)

norm_obj <- prep(norm_trans, training = biomass_tr)

transformed_te <- bake(norm_obj, biomass_te)

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
#>  1 -0.193    0.176    47.2     0.3    0.22  18.3
#>  2 -0.490    0.0342   48.1     2.85   0.34  17.6
#>  3 -0.543    0.0342   49.1     2.4    0.3   17.2
#>  4 -0.188    0.535    37.3     1.8    0.5   18.9
#>  5  0.0390   0.719    42.8     0.2    0     20.5
#>  6 -0.390    0.0342   41.7     0.7    0.2   18.5
#>  7 -0.904   -0.191    54.1     1.19   0.51  15.1
#>  8 -0.601   -0.668    33.8     0.95   0.2   16.2
#>  9 -1.84    -0.885    31.1     0.14   4.9   11.1
#> 10 -1.97    -1.41     23.7     4.63   1.05  10.8
#> # ℹ 70 more rows
tidy(norm_trans, number = 1)
#> # A tibble: 2 × 4
#>   terms    statistic value id             
#>   <chr>    <chr>     <dbl> <chr>          
#> 1 carbon   NA           NA normalize_o0ZB2
#> 2 hydrogen NA           NA normalize_o0ZB2
tidy(norm_obj, number = 1)
#> # A tibble: 4 × 4
#>   terms    statistic value id             
#>   <chr>    <chr>     <dbl> <chr>          
#> 1 carbon   mean      48.4  normalize_o0ZB2
#> 2 hydrogen mean       5.46 normalize_o0ZB2
#> 3 carbon   sd        10.4  normalize_o0ZB2
#> 4 hydrogen sd         1.20 normalize_o0ZB2

# To keep the original variables in the output, use `step_mutate_at`:
norm_keep_orig <- rec |>
  step_mutate_at(all_numeric_predictors(), fn = list(orig = ~.)) |>
  step_normalize(-contains("orig"), -all_outcomes())

keep_orig_obj <- prep(norm_keep_orig, training = biomass_tr)
keep_orig_te <- bake(keep_orig_obj, biomass_te)
keep_orig_te
#> # A tibble: 80 × 11
#>     carbon hydrogen oxygen nitrogen   sulfur   HHV carbon_orig
#>      <dbl>    <dbl>  <dbl>    <dbl>    <dbl> <dbl>       <dbl>
#>  1 -0.193    0.176   0.801  -0.643   0.00755  18.3        46.4
#>  2 -0.490    0.0342  0.881   1.47    0.281    17.6        43.2
#>  3 -0.543    0.0342  0.977   1.10    0.190    17.2        42.7
#>  4 -0.188    0.535  -0.113   0.602   0.646    18.9        46.4
#>  5  0.0390   0.719   0.392  -0.726  -0.494    20.5        48.8
#>  6 -0.390    0.0342  0.293  -0.311  -0.0380   18.5        44.3
#>  7 -0.904   -0.191   1.44    0.0958  0.668    15.1        38.9
#>  8 -0.601   -0.668  -0.436  -0.103  -0.0380   16.2        42.1
#>  9 -1.84    -0.885  -0.686  -0.776  10.7      11.1        29.2
#> 10 -1.97    -1.41   -1.37    2.95    1.90     10.8        27.8
#> # ℹ 70 more rows
#> # ℹ 4 more variables: hydrogen_orig <dbl>, oxygen_orig <dbl>,
#> #   nitrogen_orig <dbl>, sulfur_orig <dbl>
```
