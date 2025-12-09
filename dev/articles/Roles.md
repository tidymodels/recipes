# Roles in recipes

`recipes` can assign one or more roles to each column in the data. The
roles are not restricted to a predefined set; they can be anything. For
most conventional situations, they are typically “predictor” and/or
“outcome”. Additional roles enable targeted step operations on specific
variables or groups of variables.

## The Formula Method

When a recipe is created using the formula interface, this defines the
roles for all columns of the data set.
[`summary()`](https://rdrr.io/r/base/summary.html) can be used to view a
tibble containing information regarding the roles.

``` r
library(recipes)

recipe(Species ~ ., data = iris) |> summary()
#> # A tibble: 5 × 4
#>   variable     type      role      source  
#>   <chr>        <list>    <chr>     <chr>   
#> 1 Sepal.Length <chr [2]> predictor original
#> 2 Sepal.Width  <chr [2]> predictor original
#> 3 Petal.Length <chr [2]> predictor original
#> 4 Petal.Width  <chr [2]> predictor original
#> 5 Species      <chr [3]> outcome   original

recipe( ~ Species, data = iris) |> summary()
#> # A tibble: 1 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 Species  <chr [3]> predictor original

recipe(Sepal.Length + Sepal.Width ~ ., data = iris) |> summary()
#> # A tibble: 5 × 4
#>   variable     type      role      source  
#>   <chr>        <list>    <chr>     <chr>   
#> 1 Petal.Length <chr [2]> predictor original
#> 2 Petal.Width  <chr [2]> predictor original
#> 3 Species      <chr [3]> predictor original
#> 4 Sepal.Length <chr [2]> outcome   original
#> 5 Sepal.Width  <chr [2]> outcome   original
```

These roles can be updated despite this initial assignment.
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
can modify a single existing role:

``` r
library(modeldata)
data(biomass)

recipe(HHV ~ ., data = biomass) |> 
  update_role(dataset, new_role = "dataset split variable") |> 
  update_role(sample, new_role = "sample ID") |> 
  summary()
#> # A tibble: 8 × 4
#>   variable type      role                   source  
#>   <chr>    <list>    <chr>                  <chr>   
#> 1 sample   <chr [3]> sample ID              original
#> 2 dataset  <chr [3]> dataset split variable original
#> 3 carbon   <chr [2]> predictor              original
#> 4 hydrogen <chr [2]> predictor              original
#> 5 oxygen   <chr [2]> predictor              original
#> 6 nitrogen <chr [2]> predictor              original
#> 7 sulfur   <chr [2]> predictor              original
#> 8 HHV      <chr [2]> outcome                original
```

When you want to get rid of a role for a column, use
[`remove_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).

``` r
recipe(HHV ~ ., data = biomass) |> 
  remove_role(sample, old_role = "predictor") |> 
  summary()
#> # A tibble: 8 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 sample   <chr [3]> NA        original
#> 2 dataset  <chr [3]> predictor original
#> 3 carbon   <chr [2]> predictor original
#> 4 hydrogen <chr [2]> predictor original
#> 5 oxygen   <chr [2]> predictor original
#> 6 nitrogen <chr [2]> predictor original
#> 7 sulfur   <chr [2]> predictor original
#> 8 HHV      <chr [2]> outcome   original
```

It represents the lack of a role as `NA`, which means that the variable
is used in the recipe, but does not yet have a declared role. Setting
the role manually to `NA` is not allowed:

``` r
recipe(HHV ~ ., data = biomass) |> 
  update_role(sample, new_role = NA_character_)
#> Error in `update_role()`:
#> ! `new_role` must be a single string, not a character `NA`.
```

When there are cases when a column will be used in more than one
context,
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
can create additional roles:

``` r
multi_role <- recipe(HHV ~ ., data = biomass) |> 
  update_role(dataset, new_role = "dataset split variable") |> 
  update_role(sample, new_role = "sample ID") |> 
  # Roles below from https://wordcounter.net/random-word-generator
  add_role(sample, new_role = "jellyfish") 

multi_role |> 
  summary()
#> # A tibble: 9 × 4
#>   variable type      role                   source  
#>   <chr>    <list>    <chr>                  <chr>   
#> 1 sample   <chr [3]> sample ID              original
#> 2 sample   <chr [3]> jellyfish              original
#> 3 dataset  <chr [3]> dataset split variable original
#> 4 carbon   <chr [2]> predictor              original
#> 5 hydrogen <chr [2]> predictor              original
#> 6 oxygen   <chr [2]> predictor              original
#> 7 nitrogen <chr [2]> predictor              original
#> 8 sulfur   <chr [2]> predictor              original
#> 9 HHV      <chr [2]> outcome                original
```

If a variable has multiple existing roles and you want to update one of
them, the additional `old_role` argument to
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
must be used to resolve any ambiguity.

``` r
multi_role |>
  update_role(sample, new_role = "flounder", old_role = "jellyfish") |>
  summary()
#> # A tibble: 9 × 4
#>   variable type      role                   source  
#>   <chr>    <list>    <chr>                  <chr>   
#> 1 sample   <chr [3]> sample ID              original
#> 2 sample   <chr [3]> flounder               original
#> 3 dataset  <chr [3]> dataset split variable original
#> 4 carbon   <chr [2]> predictor              original
#> 5 hydrogen <chr [2]> predictor              original
#> 6 oxygen   <chr [2]> predictor              original
#> 7 nitrogen <chr [2]> predictor              original
#> 8 sulfur   <chr [2]> predictor              original
#> 9 HHV      <chr [2]> outcome                original
```

Additional variable roles allow you to use
[`has_role()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
in combination with other selection methods (see
[`?selections`](https://recipes.tidymodels.org/dev/reference/selections.md))
to target specific variables in subsequent processing steps. For
example, in the following recipe, by adding the role `"nocenter"` to the
`HHV` predictor, you can use `-has_role("nocenter")` to exclude `HHV`
when centering
[`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md).

``` r
multi_role |> 
  add_role(HHV, new_role = "nocenter") |> 
  step_center(all_predictors(), -has_role("nocenter")) |> 
  prep(training = biomass, retain = TRUE) |> 
  bake(new_data = NULL) |> 
  head()
#> # A tibble: 6 × 8
#>   sample          dataset carbon hydrogen oxygen nitrogen  sulfur   HHV
#>   <chr>           <chr>    <dbl>    <dbl>  <dbl>    <dbl>   <dbl> <dbl>
#> 1 Akhrot Shell    Traini…  1.52    0.181    4.37  -0.667  -0.234   20.0
#> 2 Alabama Oak Wo… Traini…  1.21    0.241    2.73  -0.877  -0.234   19.2
#> 3 Alder           Traini… -0.475   0.341    7.68  -0.967  -0.214   18.3
#> 4 Alfalfa         Traini… -3.19   -0.489   -2.97   2.22   -0.0736  18.2
#> 5 Alfalfa Seed S… Traini… -1.53   -0.0586   2.15  -0.0772 -0.214   18.4
#> 6 Alfalfa Stalks  Traini… -2.89    0.291    1.63   0.963  -0.134   18.5
```

The selector
[`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
can also be used in place of the compound specification above.

## The Non-Formula Interface

You can start a recipe without any roles:

``` r
recipe(biomass) |> 
  summary()
#> # A tibble: 8 × 4
#>   variable type      role  source  
#>   <chr>    <list>    <chr> <chr>   
#> 1 sample   <chr [3]> NA    original
#> 2 dataset  <chr [3]> NA    original
#> 3 carbon   <chr [2]> NA    original
#> 4 hydrogen <chr [2]> NA    original
#> 5 oxygen   <chr [2]> NA    original
#> 6 nitrogen <chr [2]> NA    original
#> 7 sulfur   <chr [2]> NA    original
#> 8 HHV      <chr [2]> NA    original
```

and roles can be added in bulk as needed:

``` r
recipe(biomass) |> 
  update_role(contains("gen"), new_role = "lunchroom") |> 
  update_role(sample, HHV, new_role = "snail") |> 
  summary()
#> # A tibble: 8 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 sample   <chr [3]> snail     original
#> 2 dataset  <chr [3]> NA        original
#> 3 carbon   <chr [2]> NA        original
#> 4 hydrogen <chr [2]> lunchroom original
#> 5 oxygen   <chr [2]> lunchroom original
#> 6 nitrogen <chr [2]> lunchroom original
#> 7 sulfur   <chr [2]> NA        original
#> 8 HHV      <chr [2]> snail     original
```

## Role Inheritance

All recipes steps have a `role` argument that lets you set the role of
*new* columns generated by the step. When a recipe modifies a column
in-place, the role is never modified. For example,
[`?step_center`](https://recipes.tidymodels.org/dev/reference/step_center.md)
has the documentation:

> `role`: Not used by this step since no new variables are created

In other cases, the roles are defaulted to a relevant value based the
context. For example,
[`?step_dummy`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
has

> `role`: For model terms created by this step, what analysis role
> should they be assigned?. By default, the function assumes that the
> binary dummy variable columns created by the original variables will
> be used as predictors in a model.

So, by default, they are predictors but don’t have to be:

``` r
recipe( ~ ., data = iris) |> 
  step_dummy(Species) |> 
  prep() |> 
  bake(new_data = NULL, all_predictors()) |> 
  dplyr::select(starts_with("Species")) |> 
  names()
#> [1] "Species_versicolor" "Species_virginica"

# or something else
recipe( ~ ., data = iris) |> 
  step_dummy(Species, role = "trousers") |> 
  prep() |> 
  bake(new_data = NULL, has_role("trousers")) |> 
  names()
#> [1] "Species_versicolor" "Species_virginica"
```
