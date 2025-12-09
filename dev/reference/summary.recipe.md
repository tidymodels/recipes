# Summarize a recipe

This function prints the current set of variables/features and some of
their characteristics.

## Usage

``` r
# S3 method for class 'recipe'
summary(object, original = FALSE, ...)
```

## Arguments

- object:

  A `recipe` object

- original:

  A logical: show the current set of variables or the original set when
  the recipe was defined.

- ...:

  further arguments passed to or from other methods (not currently
  used).

## Value

A tibble with columns `variable`, `type`, `role`, and `source`. When
`original = TRUE`, an additional column is included named
`required_to_bake` (based on the results of
[`update_role_requirements()`](https://recipes.tidymodels.org/dev/reference/update_role_requirements.md)).

## Details

Note that, until the recipe has been trained, the current and original
variables are the same.

It is possible for variables to have multiple roles by adding them with
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).
If a variable has multiple roles, it will have more than one row in the
summary tibble.

## See also

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)

## Examples

``` r
rec <- recipe(~., data = USArrests)
summary(rec)
#> # A tibble: 4 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 Murder   <chr [2]> predictor original
#> 2 Assault  <chr [2]> predictor original
#> 3 UrbanPop <chr [2]> predictor original
#> 4 Rape     <chr [2]> predictor original
rec <- step_pca(rec, all_numeric(), num_comp = 3)
summary(rec) # still the same since not yet trained
#> # A tibble: 4 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 Murder   <chr [2]> predictor original
#> 2 Assault  <chr [2]> predictor original
#> 3 UrbanPop <chr [2]> predictor original
#> 4 Rape     <chr [2]> predictor original
rec <- prep(rec, training = USArrests)
summary(rec)
#> # A tibble: 3 × 4
#>   variable type      role      source 
#>   <chr>    <list>    <chr>     <chr>  
#> 1 PC1      <chr [2]> predictor derived
#> 2 PC2      <chr [2]> predictor derived
#> 3 PC3      <chr [2]> predictor derived
```
