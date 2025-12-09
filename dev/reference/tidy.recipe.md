# Tidy the result of a recipe

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) will return a
data frame that contains information regarding a recipe or operation
within the recipe (when a
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method for
the operation exists).

## Usage

``` r
# S3 method for class 'step_BoxCox'
tidy(x, ...)

# S3 method for class 'step_YeoJohnson'
tidy(x, ...)

# S3 method for class 'step_arrange'
tidy(x, ...)

# S3 method for class 'step_bin2factor'
tidy(x, ...)

# S3 method for class 'step_bs'
tidy(x, ...)

# S3 method for class 'step_center'
tidy(x, ...)

# S3 method for class 'check_class'
tidy(x, ...)

# S3 method for class 'step_classdist'
tidy(x, ...)

# S3 method for class 'step_classdist_shrunken'
tidy(x, ...)

# S3 method for class 'check_cols'
tidy(x, ...)

# S3 method for class 'step_corr'
tidy(x, ...)

# S3 method for class 'step_count'
tidy(x, ...)

# S3 method for class 'step_cut'
tidy(x, ...)

# S3 method for class 'step_date'
tidy(x, ...)

# S3 method for class 'step_depth'
tidy(x, ...)

# S3 method for class 'step_discretize'
tidy(x, ...)

# S3 method for class 'step_dummy'
tidy(x, ...)

# S3 method for class 'step_dummy_extract'
tidy(x, ...)

# S3 method for class 'step_dummy_multi_choice'
tidy(x, ...)

# S3 method for class 'step_factor2string'
tidy(x, ...)

# S3 method for class 'step_filter'
tidy(x, ...)

# S3 method for class 'step_filter_missing'
tidy(x, ...)

# S3 method for class 'step_geodist'
tidy(x, ...)

# S3 method for class 'step_harmonic'
tidy(x, ...)

# S3 method for class 'step_holiday'
tidy(x, ...)

# S3 method for class 'step_hyperbolic'
tidy(x, ...)

# S3 method for class 'step_ica'
tidy(x, ...)

# S3 method for class 'step_impute_bag'
tidy(x, ...)

# S3 method for class 'step_impute_knn'
tidy(x, ...)

# S3 method for class 'step_impute_linear'
tidy(x, ...)

# S3 method for class 'step_impute_lower'
tidy(x, ...)

# S3 method for class 'step_impute_mean'
tidy(x, ...)

# S3 method for class 'step_impute_median'
tidy(x, ...)

# S3 method for class 'step_impute_mode'
tidy(x, ...)

# S3 method for class 'step_impute_roll'
tidy(x, ...)

# S3 method for class 'step_indicate_na'
tidy(x, ...)

# S3 method for class 'step_integer'
tidy(x, ...)

# S3 method for class 'step_interact'
tidy(x, ...)

# S3 method for class 'step_intercept'
tidy(x, ...)

# S3 method for class 'step_inverse'
tidy(x, ...)

# S3 method for class 'step_invlogit'
tidy(x, ...)

# S3 method for class 'step_isomap'
tidy(x, ...)

# S3 method for class 'step_kpca'
tidy(x, ...)

# S3 method for class 'step_kpca_poly'
tidy(x, ...)

# S3 method for class 'step_kpca_rbf'
tidy(x, ...)

# S3 method for class 'step_lag'
tidy(x, ...)

# S3 method for class 'step_lincomb'
tidy(x, ...)

# S3 method for class 'step_log'
tidy(x, ...)

# S3 method for class 'step_logit'
tidy(x, ...)

# S3 method for class 'check_missing'
tidy(x, ...)

# S3 method for class 'step_mutate'
tidy(x, ...)

# S3 method for class 'step_mutate_at'
tidy(x, ...)

# S3 method for class 'step_naomit'
tidy(x, ...)

# S3 method for class 'check_new_values'
tidy(x, ...)

# S3 method for class 'step_nnmf'
tidy(x, ...)

# S3 method for class 'step_nnmf_sparse'
tidy(x, ...)

# S3 method for class 'step_normalize'
tidy(x, ...)

# S3 method for class 'step_novel'
tidy(x, ...)

# S3 method for class 'step_ns'
tidy(x, ...)

# S3 method for class 'step_num2factor'
tidy(x, ...)

# S3 method for class 'step_nzv'
tidy(x, ...)

# S3 method for class 'step_ordinalscore'
tidy(x, ...)

# S3 method for class 'step_other'
tidy(x, ...)

# S3 method for class 'step_pca'
tidy(x, type = "coef", ...)

# S3 method for class 'step_percentile'
tidy(x, ...)

# S3 method for class 'step_pls'
tidy(x, ...)

# S3 method for class 'step_poly'
tidy(x, ...)

# S3 method for class 'step_poly_bernstein'
tidy(x, ...)

# S3 method for class 'step_profile'
tidy(x, ...)

# S3 method for class 'step_range'
tidy(x, ...)

# S3 method for class 'check_range'
tidy(x, ...)

# S3 method for class 'step_ratio'
tidy(x, ...)

# S3 method for class 'step_regex'
tidy(x, ...)

# S3 method for class 'step_relevel'
tidy(x, ...)

# S3 method for class 'step_relu'
tidy(x, ...)

# S3 method for class 'step_rename'
tidy(x, ...)

# S3 method for class 'step_rename_at'
tidy(x, ...)

# S3 method for class 'step_rm'
tidy(x, ...)

# S3 method for class 'step_sample'
tidy(x, ...)

# S3 method for class 'step_scale'
tidy(x, ...)

# S3 method for class 'step_select'
tidy(x, ...)

# S3 method for class 'step_shuffle'
tidy(x, ...)

# S3 method for class 'step_slice'
tidy(x, ...)

# S3 method for class 'step_spatialsign'
tidy(x, ...)

# S3 method for class 'step_spline_b'
tidy(x, ...)

# S3 method for class 'step_spline_convex'
tidy(x, ...)

# S3 method for class 'step_spline_monotone'
tidy(x, ...)

# S3 method for class 'step_spline_natural'
tidy(x, ...)

# S3 method for class 'step_spline_nonnegative'
tidy(x, ...)

# S3 method for class 'step_sqrt'
tidy(x, ...)

# S3 method for class 'step_string2factor'
tidy(x, ...)

# S3 method for class 'recipe'
tidy(x, number = NA, id = NA, ...)

# S3 method for class 'step'
tidy(x, ...)

# S3 method for class 'check'
tidy(x, ...)

# S3 method for class 'step_time'
tidy(x, ...)

# S3 method for class 'step_unknown'
tidy(x, ...)

# S3 method for class 'step_unorder'
tidy(x, ...)

# S3 method for class 'step_window'
tidy(x, ...)

# S3 method for class 'step_zv'
tidy(x, ...)
```

## Arguments

- x:

  A `recipe` object, step, or check (trained or otherwise).

- ...:

  Not currently used.

- type:

  For `step_pca`, either `"coef"` (for the variable loadings per
  component) or `"variance"` (how much variance does each component
  account for).

- number:

  An integer or `NA`. If missing, and `id` is not provided, the return
  value is a list of the operations in the recipe. If a number is given,
  a `tidy` method is executed for that operation in the recipe (if it
  exists). `number` must not be provided if `id` is.

- id:

  A character string or `NA`. If missing and `number` is not provided,
  the return value is a list of the operations in the recipe. If a
  character string is given, a `tidy` method is executed for that
  operation in the recipe (if it exists). `id` must not be provided if
  `number` is.

## Value

A tibble with columns that vary depending on what `tidy` method is
executed. When `number`, and `id` are `NA`, a tibble with columns
`number` (the operation iteration), `operation` (either "step" or
"check"), `type` (the method, e.g. "nzv", "center"), a logical column
called `trained` for whether the operation has been estimated using
`prep`, a logical for `skip`, and a character column `id`.

## Examples

``` r
data(Sacramento, package = "modeldata")

Sacramento_rec <- recipe(~., data = Sacramento) |>
  step_other(all_nominal(), threshold = 0.05, other = "another") |>
  step_center(all_numeric()) |>
  step_dummy(all_nominal()) |>
  check_cols(ends_with("ude"), sqft, price)

tidy(Sacramento_rec)
#> # A tibble: 4 × 6
#>   number operation type   trained skip  id          
#>    <int> <chr>     <chr>  <lgl>   <lgl> <chr>       
#> 1      1 step      other  FALSE   FALSE other_XZtdD 
#> 2      2 step      center FALSE   FALSE center_adtTz
#> 3      3 step      dummy  FALSE   FALSE dummy_VIZ8S 
#> 4      4 check     cols   FALSE   FALSE cols_HEnNH  

tidy(Sacramento_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms         value id          
#>   <chr>         <dbl> <chr>       
#> 1 all_numeric()    NA center_adtTz
tidy(Sacramento_rec, number = 3)
#> # A tibble: 1 × 3
#>   terms         columns id         
#>   <chr>         <chr>   <chr>      
#> 1 all_nominal() NA      dummy_VIZ8S

Sacramento_rec_trained <- prep(Sacramento_rec, training = Sacramento)

tidy(Sacramento_rec_trained)
#> # A tibble: 4 × 6
#>   number operation type   trained skip  id          
#>    <int> <chr>     <chr>  <lgl>   <lgl> <chr>       
#> 1      1 step      other  TRUE    FALSE other_XZtdD 
#> 2      2 step      center TRUE    FALSE center_adtTz
#> 3      3 step      dummy  TRUE    FALSE dummy_VIZ8S 
#> 4      4 check     cols   TRUE    FALSE cols_HEnNH  
tidy(Sacramento_rec_trained, number = 3)
#> # A tibble: 6 × 3
#>   terms columns     id         
#>   <chr> <chr>       <chr>      
#> 1 city  ROSEVILLE   dummy_VIZ8S
#> 2 city  SACRAMENTO  dummy_VIZ8S
#> 3 city  another     dummy_VIZ8S
#> 4 zip   another     dummy_VIZ8S
#> 5 type  Residential dummy_VIZ8S
#> 6 type  another     dummy_VIZ8S
tidy(Sacramento_rec_trained, number = 4)
#> # A tibble: 4 × 2
#>   terms     id        
#>   <chr>     <chr>     
#> 1 latitude  cols_HEnNH
#> 2 longitude cols_HEnNH
#> 3 sqft      cols_HEnNH
#> 4 price     cols_HEnNH
```
