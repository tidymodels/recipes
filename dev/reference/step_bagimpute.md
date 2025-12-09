# Impute via bagged trees

**\[deprecated\]**

Please use
[`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md)
instead.

## Usage

``` r
step_bagimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  impute_with = imp_vars(all_predictors()),
  trees = 25,
  models = NULL,
  options = list(keepX = FALSE),
  seed_val = sample.int(10^4, 1),
  skip = FALSE,
  id = rand_id("impute_bag")
)
```
