# Impute numeric data below the threshold of measurement

**\[deprecated\]**

Please use
[`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md)
instead.

## Usage

``` r
step_lowerimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = NULL,
  skip = FALSE,
  id = rand_id("impute_lower")
)
```
