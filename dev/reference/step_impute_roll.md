# Impute numeric data using a rolling window statistic

`step_impute_roll()` creates a *specification* of a recipe step that
will substitute missing values of numeric variables by the measure of
location (e.g. median) within a moving window.

## Usage

``` r
step_impute_roll(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  statistic = median,
  window = 5L,
  skip = FALSE,
  id = rand_id("impute_roll")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables to be imputed;
  these columns must be non-integer numerics (i.e., double precision).
  See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- statistic:

  A function with a single argument for the data to compute the imputed
  value. Only complete values will be passed to the function and it
  should return a double precision value.

- window:

  The size of the window around a point to be imputed. Should be an odd
  integer greater than one. See Details below for a discussion of points
  at the ends of the series.

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

On the tails, the window is shifted towards the ends. For example, for a
5-point window, the windows for the first four points are `1:5`, `1:5`,
`1:5`, and then `2:6`.

When missing data are in the window, they are not passed to the
function. If all of the data in the window are missing, a missing value
is returned.

The statistics are calculated on the training set values *before*
imputation. This means that if previous data within the window are
missing, their imputed values are not included in the window data used
for imputation. In other words, each imputation does not know anything
about previous imputations in the series prior to the current point.

As of `recipes` 0.1.16, this function name changed from
[`step_rollimpute()`](https://recipes.tidymodels.org/dev/reference/step_rollimpute.md)
to `step_impute_roll()`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `window` , and
`id`:

- terms:

  character, the selectors or variables selected

- window:

  integer, window size

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `statistic`: Rolling Summary Statistic (type: character, default:
  median)

- `window`: Window Size (type: integer, default: 5)

## Case weights

The underlying operation does not allow for case weights.

## See also

Other imputation steps:
[`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md),
[`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md),
[`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md),
[`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md),
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
[`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md)

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
library(lubridate)

set.seed(145)
example_data <-
  data.frame(
    day = ymd("2012-06-07") + days(1:12),
    x1 = round(runif(12), 2),
    x2 = round(runif(12), 2),
    x3 = round(runif(12), 2)
  )
example_data$x1[c(1, 5, 6)] <- NA
example_data$x2[c(1:4, 10)] <- NA

library(recipes)
seven_pt <- recipe(~., data = example_data) |>
  update_role(day, new_role = "time_index") |>
  step_impute_roll(all_numeric_predictors(), window = 7) |>
  prep(training = example_data)

# The training set:
bake(seven_pt, new_data = NULL)
#> # A tibble: 12 × 4
#>    day           x1    x2    x3
#>    <date>     <dbl> <dbl> <dbl>
#>  1 2012-06-08  0.89 0.79   0.58
#>  2 2012-06-09  0.53 0.79   0.45
#>  3 2012-06-10  0.86 0.79   0.67
#>  4 2012-06-11  0.92 0.79   0.05
#>  5 2012-06-12  0.86 0.88   0.27
#>  6 2012-06-13  0.86 0.6    0.13
#>  7 2012-06-14  0.97 0.79   0.67
#>  8 2012-06-15  0.85 0.27   0.16
#>  9 2012-06-16  0.86 0.11   0.36
#> 10 2012-06-17  0.96 0.435  0.09
#> 11 2012-06-18  0.91 0.16   0.76
#> 12 2012-06-19  0.07 0.86   0.74
```
