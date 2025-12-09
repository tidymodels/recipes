# Impute numeric data using a rolling window statistic

**\[deprecated\]**

Please use
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md)
instead.

## Usage

``` r
step_rollimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  statistic = median,
  window = 5,
  skip = FALSE,
  id = rand_id("impute_roll")
)
```
