# Introduction to recipes

This document demonstrates some basic uses of recipes. First, some
definitions are required:

- **variables** are the original (raw) data columns in a data frame or
  tibble. For example, in a traditional formula `Y ~ A + B + A:B`, the
  variables are `A`, `B`, and `Y`.
- **roles** define how variables will be used in the model. Examples
  are: `predictor` (independent variables), `response`, and
  `case weight`. This is meant to be open-ended and extensible.
- **terms** are columns in a design matrix such as `A`, `B`, and `A:B`.
  These can be other derived entities that are grouped, such as a set of
  principal components or a set of columns, that define a basis function
  for a variable. These are synonymous with features in machine
  learning. Variables that have `predictor` roles would automatically be
  main effect terms.

## An Example

The packages contains a data set used to predict whether a person will
pay back a bank loan. It has 13 predictor columns and a factor variable
`Status` (the outcome). We will first separate the data into a training
and test set:

``` r
library(recipes)
library(rsample)
library(modeldata)

data("credit_data")

set.seed(55)
train_test_split <- initial_split(credit_data)

credit_train <- training(train_test_split)
credit_test <- testing(train_test_split)
```

Note that there are some missing values in these data:

``` r
vapply(credit_train, function(x) mean(!is.na(x)), numeric(1))
#>    Status Seniority      Home      Time       Age   Marital   Records 
#>     1.000     1.000     0.998     1.000     1.000     1.000     1.000 
#>       Job  Expenses    Income    Assets      Debt    Amount     Price 
#>     0.999     1.000     0.910     0.989     0.996     1.000     1.000
```

Rather than remove these, their values will be imputed.

The idea is that the preprocessing operations will all be created using
the training set and then these steps will be applied to both the
training and test set.

## An Initial Recipe

First, we will create a recipe object from the original data and then
specify the processing steps.

Recipes can be created manually by sequentially adding roles to
variables in a data set.

If the analysis only requires **outcomes** and **predictors**, the
easiest way to create the initial recipe is to use the standard formula
method:

``` r
rec_obj <- recipe(Status ~ ., data = credit_train)
rec_obj
```

The data contained in the `data` argument need not be the training set;
this data is only used to catalog the names of the variables and their
types (e.g. numeric, etc.).

(Note that the formula method is used here to declare the variables,
their roles and nothing else. If you use inline functions (e.g. `log`)
it will complain. These types of operations can be added later.)

## Preprocessing Steps

From here, preprocessing steps for some step *X* can be added
sequentially in one of two ways:

``` r
rec_obj <- step_{X}(rec_obj, arguments)    ## or
rec_obj <- rec_obj |> step_{X}(arguments)
```

`step_dummy` and the other functions will always return updated recipes.

One other important facet of the code is the method for specifying which
variables should be used in different steps. The manual page
[`?selections`](https://recipes.tidymodels.org/dev/reference/selections.md)
has more details but
[`dplyr`](https://cran.r-project.org/package=dplyr)-like selector
functions can be used:

- use basic variable names (e.g. `x1, x2`),
- [`dplyr`](https://cran.r-project.org/package=dplyr) functions for
  selecting variables:
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  and
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
- functions that subset on the role of the variables that have been
  specified so far:
  [`all_outcomes()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`has_role()`](https://recipes.tidymodels.org/dev/reference/has_role.md),  
- similar functions for the type of data:
  [`all_nominal()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`all_numeric()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  and
  [`has_type()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  or
- compound selectors such as
  [`all_nominal_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  or
  [`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md).

Note that the methods listed above are the only ones that can be used to
select variables inside the steps. Also, minus signs can be used to
deselect variables.

For our data, we can add an operation to impute the predictors. There
are many ways to do this and `recipes` includes a few steps for this
purpose:

``` r
grep("impute_", ls("package:recipes"), value = TRUE)
#> [1] "step_impute_bag"    "step_impute_knn"    "step_impute_linear"
#> [4] "step_impute_lower"  "step_impute_mean"   "step_impute_median"
#> [7] "step_impute_mode"   "step_impute_roll"
```

Here, *K*-nearest neighbor imputation will be used. This works for both
numeric and non-numeric predictors and defaults *K* to five To do this,
it selects all predictors and then removes those that are numeric:

``` r
imputed <- rec_obj |>
  step_impute_knn(all_predictors()) 
imputed
```

It is important to realize that the *specific* variables have not been
declared yet (as shown when the recipe is printed above). In some
preprocessing steps, variables will be added or removed from the current
list of possible variables.

Since some predictors are categorical in nature (i.e. nominal), it would
make sense to convert these factor predictors into numeric dummy
variables (aka indicator variables) using
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md).
To do this, the step selects all non-numeric predictors:

``` r
ind_vars <- imputed |>
  step_dummy(all_nominal_predictors()) 
ind_vars
```

At this point in the recipe, all of the predictor should be encoded as
numeric, we can further add more steps to center and scale them:

``` r
standardized <- ind_vars |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) 
standardized
```

If these are the only preprocessing steps for the predictors, we can now
estimate the means and standard deviations from the training set. The
`prep` function is used with a recipe and a data set:

``` r
trained_rec <- prep(standardized, training = credit_train)
trained_rec
```

Note that the real variables are listed (e.g. `Home` etc.) instead of
the selectors
([`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)).

Now that the statistics have been estimated, the preprocessing can be
*applied* to the training and test set:

``` r
train_data <- bake(trained_rec, new_data = credit_train)
test_data  <- bake(trained_rec, new_data = credit_test)
```

`bake` returns a tibble that, by default, includes all of the variables:

``` r
class(test_data)
#> [1] "tbl_df"     "tbl"        "data.frame"
test_data
#> # A tibble: 1,114 × 23
#>    Seniority   Time    Age Expenses  Income Assets   Debt  Amount
#>        <dbl>  <dbl>  <dbl>    <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
#>  1     1.09   0.924  1.88    -0.385 -0.131  -0.488 -0.295 -0.0817
#>  2    -0.977  0.924 -0.459    1.77  -0.437   0.845 -0.295  0.333 
#>  3    -0.977  0.103  0.349    1.77  -0.783  -0.488 -0.295  0.333 
#>  4    -0.247  0.103 -0.280    0.231 -0.207  -0.133 -0.295  0.229 
#>  5    -0.125 -0.718 -0.729    0.231 -0.258  -0.222 -0.295 -0.807 
#>  6    -0.855  0.924 -0.549   -1.05  -0.0539 -0.488 -0.295  0.436 
#>  7     2.31   0.924  0.349    0.949 -0.0155 -0.488 -0.295 -0.185 
#>  8     0.848 -0.718  0.529    1.00   1.40   -0.133 -0.295  1.58  
#>  9    -0.977 -0.718 -1.27    -0.538 -0.246  -0.266 -0.295 -1.32  
#> 10    -0.855  0.514 -0.100    0.744 -0.540  -0.488 -0.295 -0.185 
#> # ℹ 1,104 more rows
#> # ℹ 15 more variables: Price <dbl>, Status <fct>, Home_other <dbl>,
#> #   Home_owner <dbl>, Home_parents <dbl>, Home_priv <dbl>,
#> #   Home_rent <dbl>, Marital_married <dbl>, Marital_separated <dbl>,
#> #   Marital_single <dbl>, Marital_widow <dbl>, Records_yes <dbl>,
#> #   Job_freelance <dbl>, Job_others <dbl>, Job_partime <dbl>
vapply(test_data, function(x) mean(!is.na(x)), numeric(1))
#>         Seniority              Time               Age 
#>                 1                 1                 1 
#>          Expenses            Income            Assets 
#>                 1                 1                 1 
#>              Debt            Amount             Price 
#>                 1                 1                 1 
#>            Status        Home_other        Home_owner 
#>                 1                 1                 1 
#>      Home_parents         Home_priv         Home_rent 
#>                 1                 1                 1 
#>   Marital_married Marital_separated    Marital_single 
#>                 1                 1                 1 
#>     Marital_widow       Records_yes     Job_freelance 
#>                 1                 1                 1 
#>        Job_others       Job_partime 
#>                 1                 1
```

Selectors can also be used. For example, if only the predictors are
needed, you can use `bake(object, new_data, all_predictors())`.

There are a number of other steps included in the package:

    #>  [1] "step_arrange"            "step_bagimpute"         
    #>  [3] "step_bin2factor"         "step_BoxCox"            
    #>  [5] "step_bs"                 "step_center"            
    #>  [7] "step_classdist"          "step_classdist_shrunken"
    #>  [9] "step_corr"               "step_count"             
    #> [11] "step_cut"                "step_date"              
    #> [13] "step_depth"              "step_discretize"        
    #> [15] "step_dummy"              "step_dummy_extract"     
    #> [17] "step_dummy_multi_choice" "step_factor2string"     
    #> [19] "step_filter"             "step_filter_missing"    
    #> [21] "step_geodist"            "step_harmonic"          
    #> [23] "step_holiday"            "step_hyperbolic"        
    #> [25] "step_ica"                "step_impute_bag"        
    #> [27] "step_impute_knn"         "step_impute_linear"     
    #> [29] "step_impute_lower"       "step_impute_mean"       
    #> [31] "step_impute_median"      "step_impute_mode"       
    #> [33] "step_impute_roll"        "step_indicate_na"       
    #> [35] "step_integer"            "step_interact"          
    #> [37] "step_intercept"          "step_inverse"           
    #> [39] "step_invlogit"           "step_isomap"            
    #> [41] "step_knnimpute"          "step_kpca"              
    #> [43] "step_kpca_poly"          "step_kpca_rbf"          
    #> [45] "step_lag"                "step_lincomb"           
    #> [47] "step_log"                "step_logit"             
    #> [49] "step_lowerimpute"        "step_meanimpute"        
    #> [51] "step_medianimpute"       "step_modeimpute"        
    #> [53] "step_mutate"             "step_mutate_at"         
    #> [55] "step_naomit"             "step_nnmf"              
    #> [57] "step_nnmf_sparse"        "step_normalize"         
    #> [59] "step_novel"              "step_ns"                
    #> [61] "step_num2factor"         "step_nzv"               
    #> [63] "step_ordinalscore"       "step_other"             
    #> [65] "step_pca"                "step_percentile"        
    #> [67] "step_pls"                "step_poly"              
    #> [69] "step_poly_bernstein"     "step_profile"           
    #> [71] "step_range"              "step_ratio"             
    #> [73] "step_regex"              "step_relevel"           
    #> [75] "step_relu"               "step_rename"            
    #> [77] "step_rename_at"          "step_rm"                
    #> [79] "step_rollimpute"         "step_sample"            
    #> [81] "step_scale"              "step_select"            
    #> [83] "step_shuffle"            "step_slice"             
    #> [85] "step_spatialsign"        "step_spline_b"          
    #> [87] "step_spline_convex"      "step_spline_monotone"   
    #> [89] "step_spline_natural"     "step_spline_nonnegative"
    #> [91] "step_sqrt"               "step_string2factor"     
    #> [93] "step_time"               "step_unknown"           
    #> [95] "step_unorder"            "step_window"            
    #> [97] "step_YeoJohnson"         "step_zv"

## Checks

Another type of operation that can be added to a recipes is a *check*.
Checks conduct some sort of data validation and, if no issue is found,
returns the data as-is; otherwise, an error is thrown.

For example, `check_missing` will fail if any of the variables selected
for validation have missing values. This check is done when the recipe
is prepared as well as when any data are baked. Checks are added in the
same way as steps:

``` r
trained_rec <- trained_rec |>
  check_missing(contains("Marital"))
```

Currently, `recipes` includes:

    #> [1] "check_class"      "check_cols"       "check_missing"   
    #> [4] "check_name"       "check_new_data"   "check_new_values"
    #> [7] "check_options"    "check_range"      "check_type"
