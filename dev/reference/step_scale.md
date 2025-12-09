# Scaling numeric data

`step_scale()` creates a *specification* of a recipe step that will
normalize numeric data to have a standard deviation of one.

## Usage

``` r
step_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  sds = NULL,
  factor = 1,
  na_rm = TRUE,
  skip = FALSE,
  id = rand_id("scale")
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

- sds:

  A named numeric vector of standard deviations. This is `NULL` until
  computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- factor:

  A numeric value of either 1 or 2 that scales the numeric inputs by one
  or two standard deviations. By dividing by two standard deviations,
  the coefficients attached to continuous predictors can be interpreted
  the same way as with binary inputs. Defaults to `1`. More in reference
  below.

- na_rm:

  A logical value indicating whether `NA` values should be removed when
  computing the standard deviation.

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

Scaling data means that the standard deviation of a variable is divided
out of the data. `step_scale()` estimates the variable standard
deviations from the data used in the `training` argument of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) then
applies the scaling to new data sets using these standard deviations.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the standard deviations

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## References

Gelman, A. (2007) "Scaling regression inputs by dividing by two standard
deviations." Unpublished. Source:
`https://sites.stat.columbia.edu/gelman/research/unpublished/standardizing.pdf`.

## See also

Other normalization steps:
[`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md),
[`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md),
[`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md)

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

scaled_trans <- rec |>
  step_scale(carbon, hydrogen)

scaled_obj <- prep(scaled_trans, training = biomass_tr)

transformed_te <- bake(scaled_obj, biomass_te)

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
#>    carbon hydrogen oxygen nitrogen sulfur   HHV
#>     <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <dbl>
#>  1   4.45     4.74   47.2     0.3    0.22  18.3
#>  2   4.16     4.60   48.1     2.85   0.34  17.6
#>  3   4.10     4.60   49.1     2.4    0.3   17.2
#>  4   4.46     5.10   37.3     1.8    0.5   18.9
#>  5   4.68     5.28   42.8     0.2    0     20.5
#>  6   4.26     4.60   41.7     0.7    0.2   18.5
#>  7   3.74     4.37   54.1     1.19   0.51  15.1
#>  8   4.04     3.89   33.8     0.95   0.2   16.2
#>  9   2.81     3.68   31.1     0.14   4.9   11.1
#> 10   2.67     3.15   23.7     4.63   1.05  10.8
#> # ℹ 70 more rows
tidy(scaled_trans, number = 1)
#> # A tibble: 2 × 3
#>   terms    value id         
#>   <chr>    <dbl> <chr>      
#> 1 carbon      NA scale_c85ah
#> 2 hydrogen    NA scale_c85ah
tidy(scaled_obj, number = 1)
#> # A tibble: 2 × 3
#>   terms    value id         
#>   <chr>    <dbl> <chr>      
#> 1 carbon   10.4  scale_c85ah
#> 2 hydrogen  1.20 scale_c85ah
```
