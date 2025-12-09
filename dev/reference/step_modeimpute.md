# Impute nominal data using the most common value

**\[deprecated\]**

Please use
[`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md)
instead.

## Usage

``` r
step_modeimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  modes = NULL,
  ptype = NULL,
  skip = FALSE,
  id = rand_id("impute_mode")
)
```
