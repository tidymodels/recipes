# Selecting variables

When recipe steps are used, there are different approaches that can be
used to select which variables or features should be used.

The three main characteristics of variables that can be queried:

- the name of the variable
- the data type (e.g. numeric or nominal)
- the role that was declared by the recipe

The manual pages for
[`?selections`](https://recipes.tidymodels.org/dev/reference/selections.md)
and
[`?has_role`](https://recipes.tidymodels.org/dev/reference/has_role.md)
have details about the available selection methods.

To illustrate this, the palmer penguins data will be used:

``` r
library(recipes)
library(modeldata)

data("penguins")
str(penguins)
#> tibble [344 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ species          : Factor w/ 3 levels "Adelie","Chinstrap",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ island           : Factor w/ 3 levels "Biscoe","Dream",..: 3 3 3 3 3 3 3 3 3 3 ...
#>  $ bill_length_mm   : num [1:344] 39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
#>  $ bill_depth_mm    : num [1:344] 18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
#>  $ flipper_length_mm: int [1:344] 181 186 195 NA 193 190 181 195 193 190 ...
#>  $ body_mass_g      : int [1:344] 3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...
#>  $ sex              : Factor w/ 2 levels "female","male": 2 1 1 NA 1 2 1 2 NA NA ...

rec <- recipe(body_mass_g ~ ., data = penguins)
rec
```

Before any steps are used the information on the original variables is:

``` r
summary(rec, original = TRUE)
#> # A tibble: 7 × 5
#>   variable          type      role      source   required_to_bake
#>   <chr>             <list>    <chr>     <chr>    <lgl>           
#> 1 species           <chr [3]> predictor original TRUE            
#> 2 island            <chr [3]> predictor original TRUE            
#> 3 bill_length_mm    <chr [2]> predictor original TRUE            
#> 4 bill_depth_mm     <chr [2]> predictor original TRUE            
#> 5 flipper_length_mm <chr [2]> predictor original TRUE            
#> 6 sex               <chr [3]> predictor original TRUE            
#> 7 body_mass_g       <chr [2]> outcome   original FALSE
```

This shows the types and roles. Each variable can have one or more
types, so we can printing them out seperately

``` r
summary(rec, original = TRUE)$type
#> [[1]]
#> [1] "factor"    "unordered" "nominal"  
#> 
#> [[2]]
#> [1] "factor"    "unordered" "nominal"  
#> 
#> [[3]]
#> [1] "double"  "numeric"
#> 
#> [[4]]
#> [1] "double"  "numeric"
#> 
#> [[5]]
#> [1] "integer" "numeric"
#> 
#> [[6]]
#> [1] "factor"    "unordered" "nominal"  
#> 
#> [[7]]
#> [1] "integer" "numeric"
```

Notice that integer variables have roles `"integer"` and `"numeric"`,
and the factor variables have roles `"factor"`, `"unordered"`,
`"nominal"`. This allows for some neat selections where the selector
[`all_numeric()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
select double and integer variables, and more specific selectors such as
[`all_integer()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
only select integer variables. A full hierarchy of types can be seen in
[`?has_role`](https://recipes.tidymodels.org/dev/reference/has_role.md).

We can add a step to normalize numeric data:

``` r
dummied <- rec |> step_normalize(all_numeric())
```

This will capture *any* variables that are either character integers or
doubles: `bill_length_mm`, `bill_depth_mm`, `flipper_length_mm` and
`body_mass_g`. However, since `body_mass_g` is our outcome, we might
want to keep it as a factor so we can *subtract* that variable out
either by name or by role:

``` r
dummied <- rec |> step_normalize(bill_length_mm, bill_depth_mm, 
                                  flipper_length_mm) # or
dummied <- rec |> step_normalize(all_numeric(), - body_mass_g) # or
dummied <- rec |> step_normalize(all_numeric_predictors()) # recommended
```

Whenever possible, it is recommended to use the more specific
`*_predictors()` variants to avoid accidentally selecting the outcomes.

``` r
rec |>
  step_dummy(sex) |>
  prep() |>
  juice()
#> Warning: ! There are new levels in `sex`: NA.
#> ℹ Consider using step_unknown() (`?recipes::step_unknown()`) before
#>   `step_dummy()` to handle missing values.
#> # A tibble: 344 × 7
#>    species island    bill_length_mm bill_depth_mm flipper_length_mm
#>    <fct>   <fct>              <dbl>         <dbl>             <int>
#>  1 Adelie  Torgersen           39.1          18.7               181
#>  2 Adelie  Torgersen           39.5          17.4               186
#>  3 Adelie  Torgersen           40.3          18                 195
#>  4 Adelie  Torgersen           NA            NA                  NA
#>  5 Adelie  Torgersen           36.7          19.3               193
#>  6 Adelie  Torgersen           39.3          20.6               190
#>  7 Adelie  Torgersen           38.9          17.8               181
#>  8 Adelie  Torgersen           39.2          19.6               195
#>  9 Adelie  Torgersen           34.1          18.1               193
#> 10 Adelie  Torgersen           42            20.2               190
#> # ℹ 334 more rows
#> # ℹ 2 more variables: body_mass_g <int>, sex_male <dbl>
```

Using the last definition:

``` r
dummied <- prep(dummied, training = penguins)
with_dummy <- bake(dummied, new_data = penguins)
with_dummy
#> # A tibble: 344 × 7
#>    species island  bill_length_mm bill_depth_mm flipper_length_mm sex  
#>    <fct>   <fct>            <dbl>         <dbl>             <dbl> <fct>
#>  1 Adelie  Torger…         -0.883         0.784            -1.42  male 
#>  2 Adelie  Torger…         -0.810         0.126            -1.06  fema…
#>  3 Adelie  Torger…         -0.663         0.430            -0.421 fema…
#>  4 Adelie  Torger…         NA            NA                NA     NA   
#>  5 Adelie  Torger…         -1.32          1.09             -0.563 fema…
#>  6 Adelie  Torger…         -0.847         1.75             -0.776 male 
#>  7 Adelie  Torger…         -0.920         0.329            -1.42  fema…
#>  8 Adelie  Torger…         -0.865         1.24             -0.421 male 
#>  9 Adelie  Torger…         -1.80          0.480            -0.563 NA   
#> 10 Adelie  Torger…         -0.352         1.54             -0.776 NA   
#> # ℹ 334 more rows
#> # ℹ 1 more variable: body_mass_g <int>
```

`body_mass_g` is unaffected.

One important aspect of selecting variables in steps is that the
variable names and types may change as steps are being executed. In the
above example, `sex` is a factor variable, if
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
was used on it, then `sex` would be removed and the binary variable
`sex_male` is in its place. One reason to have general selection
routines like
[`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
or
[`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html)
is to be able to select variables that have not been created yet.

All steps in the recipes package support empty selections. Meaning that
if
[`all_date_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
is used in a step, and no date variables was found the in the data set,
then the step is applied without error. The calculations inside the step
will be skipped. This allows for quite relaxed recipes as you don’t have
to make sure that the variables exists at that point in the recipe.
