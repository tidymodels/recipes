# Impute numeric data using the mean

**\[deprecated\]**

Please use
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md)
instead.

## Usage

``` r
step_meanimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  means = NULL,
  trim = 0,
  skip = FALSE,
  id = rand_id("impute_mean")
)
```
