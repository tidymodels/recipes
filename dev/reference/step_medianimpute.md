# Impute numeric data using the median

**\[deprecated\]**

Please use
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md)
instead.

## Usage

``` r
step_medianimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  medians = NULL,
  skip = FALSE,
  id = rand_id("impute_median")
)
```
