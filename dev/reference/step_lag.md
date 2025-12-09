# Create a lagged predictor

`step_lag()` creates a *specification* of a recipe step that will add
new columns of lagged data. Lagged data will by default include NA
values where the lag was induced. These can be removed with
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
or you may specify an alternative filler value with the `default`
argument.

## Usage

``` r
step_lag(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  lag = 1,
  prefix = "lag_",
  default = NA,
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("lag")
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

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- lag:

  A vector of positive integers. Each specified column will be lagged
  for each value in the vector.

- prefix:

  A prefix for generated column names, default to `"lag_"`.

- default:

  Passed to
  [`dplyr::lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html),
  determines what fills empty rows left by lagging (defaults to NA).

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

The step assumes that the data are already *in the proper sequential
order* for lagging.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
n <- 10
start <- as.Date("1999/01/01")
end <- as.Date("1999/01/10")

df <- data.frame(
  x = runif(n),
  index = 1:n,
  day = seq(start, end, by = "day")
)

recipe(~., data = df) |>
  step_lag(index, day, lag = 2:3) |>
  prep(df) |>
  bake(df)
#> # A tibble: 10 × 7
#>        x index day        lag_2_index lag_3_index lag_2_day  lag_3_day 
#>    <dbl> <int> <date>           <int>       <int> <date>     <date>    
#>  1 0.332     1 1999-01-01          NA          NA NA         NA        
#>  2 0.210     2 1999-01-02          NA          NA NA         NA        
#>  3 0.270     3 1999-01-03           1          NA 1999-01-01 NA        
#>  4 0.249     4 1999-01-04           2           1 1999-01-02 1999-01-01
#>  5 0.872     5 1999-01-05           3           2 1999-01-03 1999-01-02
#>  6 0.253     6 1999-01-06           4           3 1999-01-04 1999-01-03
#>  7 0.448     7 1999-01-07           5           4 1999-01-05 1999-01-04
#>  8 0.245     8 1999-01-08           6           5 1999-01-06 1999-01-05
#>  9 0.575     9 1999-01-09           7           6 1999-01-07 1999-01-06
#> 10 0.111    10 1999-01-10           8           7 1999-01-08 1999-01-07
```
