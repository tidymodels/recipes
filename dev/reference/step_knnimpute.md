# Impute via k-nearest neighbors

**\[deprecated\]**

Please use
[`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md)
instead.

## Usage

``` r
step_knnimpute(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  neighbors = 5,
  impute_with = imp_vars(all_predictors()),
  options = list(nthread = 1, eps = 1e-08),
  ref_data = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("impute_knn")
)
```
