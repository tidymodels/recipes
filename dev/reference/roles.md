# Manually alter roles

`update_role()` alters an existing role in the recipe or assigns an
initial role to variables that do not yet have a declared role.

`add_role()` adds an *additional* role to variables that already have a
role in the recipe. It does not overwrite old roles, as a single
variable can have multiple roles.

`remove_role()` eliminates a single existing role in the recipe.

## Usage

``` r
add_role(recipe, ..., new_role = "predictor", new_type = NULL)

update_role(recipe, ..., new_role = "predictor", old_role = NULL)

remove_role(recipe, ..., old_role)
```

## Arguments

- recipe:

  An existing
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).

- ...:

  One or more selector functions to choose which variables are being
  assigned a role. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- new_role:

  A character string for a single role.

- new_type:

  A character string for specific type that the variable should be
  identified as. If left as `NULL`, the type is automatically identified
  as the *first* type you see for that variable in `summary(recipe)`.

- old_role:

  A character string for the specific role to update for the variables
  selected by `...`. `update_role()` accepts a `NULL` as long as the
  variables have only a single role.

## Value

An updated recipe object.

## Details

`update_role()`, `add_role()` and `remove_role()` will be applied on a
recipe before any of the steps or checks, regardless of where they are
located in position. This means that roles can only be changed with
these three functions for columns that are already present in the
original data supplied to
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).
See the `role` argument in some step functions to update roles for
columns created by steps.

Variables can have any arbitrary role (see the examples) but there are
three special standard roles, `"predictor"`, `"outcome"`, and
`"case_weights"`. The first two roles are typically required when
fitting a model.

`update_role()` should be used when a variable doesn't currently have a
role in the recipe, or to replace an `old_role` with a `new_role`.
`add_role()` only adds additional roles to variables that already have
roles and will throw an error when the current role is missing (i.e.
`NA`).

When using `add_role()`, if a variable is selected that already has the
`new_role`, a warning is emitted and that variable is skipped so no
duplicate roles are added.

Adding or updating roles is a useful way to group certain variables that
don't fall in the standard `"predictor"` bucket. You can perform a step
on all of the variables that have a custom role with the selector
[`has_role()`](https://recipes.tidymodels.org/dev/reference/has_role.md).

### Effects of non-standard roles

Recipes can label and retain column(s) of your data set that should not
be treated as outcomes or predictors. A unique identifier column or some
other ancillary data could be used to troubleshoot issues during model
development but may not be either an outcome or predictor.

For example, the
[`modeldata::biomass`](https://modeldata.tidymodels.org/reference/biomass.html)
dataset has a column named `sample` with information about the specific
sample type. We can change that role:

    library(recipes)

    data(biomass, package = "modeldata")
    biomass_train <- biomass[1:100,]
    biomass_test <- biomass[101:200,]

    rec <- recipe(HHV ~ ., data = biomass_train) |>
      update_role(sample, new_role = "id variable") |>
      step_center(carbon)

    rec <- prep(rec, biomass_train)

This means that `sample` is no longer treated as a `"predictor"` (the
default role for columns on the right-hand side of the formula supplied
to [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md))
and won't be used in model fitting or analysis, but will still be
retained in the data set.

If you really aren't using `sample` in your recipe, we recommend that
you instead remove `sample` from your dataset before passing it to
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).
The reason for this is because recipes assumes that all non-standard
roles are required at
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time
(or [`predict()`](https://rdrr.io/r/stats/predict.html) time, if you are
using a workflow). Since you didn't use `sample` in any steps of the
recipe, you might think that you don't need to pass it to
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md), but
this isn't true because recipes doesn't know that you didn't use it:

    biomass_test$sample <- NULL

    bake(rec, biomass_test)
    #> Error in `bake()`:
    #> x The following required columns are missing from `new_data`: `sample`.
    #> i These columns have one of the following roles, which are required at `bake()`
    #>   time: `id variable`.
    #> i If these roles are not required at `bake()` time, use
    #>   `update_role_requirements(role = "your_role", bake = FALSE)`.

As we mentioned before, the best way to avoid this issue is to not even
use a role, just remove the `sample` column from `biomass` before
calling
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md). In
general, predictors and non-standard roles that are supplied to
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
should be present at both
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time.

If you can't remove `sample` for some reason, then the second best way
to get around this issue is to tell recipes that the `"id variable"`
role isn't required at
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time.
You can do that by using
[`update_role_requirements()`](https://recipes.tidymodels.org/dev/reference/update_role_requirements.md):

    rec <- recipe(HHV ~ ., data = biomass_train) |>
      update_role(sample, new_role = "id variable") |>
      update_role_requirements("id variable", bake = FALSE) |>
      step_center(carbon)

    rec <- prep(rec, biomass_train)

    # No errors!
    biomass_test_baked <- bake(rec, biomass_test)

It should be very rare that you need this feature.

## Examples

``` r
library(recipes)
data(biomass, package = "modeldata")

# Using the formula method, roles are created for any outcomes and predictors:
recipe(HHV ~ ., data = biomass) |>
  summary()
#> # A tibble: 8 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 sample   <chr [3]> predictor original
#> 2 dataset  <chr [3]> predictor original
#> 3 carbon   <chr [2]> predictor original
#> 4 hydrogen <chr [2]> predictor original
#> 5 oxygen   <chr [2]> predictor original
#> 6 nitrogen <chr [2]> predictor original
#> 7 sulfur   <chr [2]> predictor original
#> 8 HHV      <chr [2]> outcome   original

# However `sample` and `dataset` aren't predictors. Since they already have
# roles, `update_role()` can be used to make changes, to any arbitrary role:
recipe(HHV ~ ., data = biomass) |>
  update_role(sample, new_role = "id variable") |>
  update_role(dataset, new_role = "splitting variable") |>
  summary()
#> # A tibble: 8 × 4
#>   variable type      role               source  
#>   <chr>    <list>    <chr>              <chr>   
#> 1 sample   <chr [3]> id variable        original
#> 2 dataset  <chr [3]> splitting variable original
#> 3 carbon   <chr [2]> predictor          original
#> 4 hydrogen <chr [2]> predictor          original
#> 5 oxygen   <chr [2]> predictor          original
#> 6 nitrogen <chr [2]> predictor          original
#> 7 sulfur   <chr [2]> predictor          original
#> 8 HHV      <chr [2]> outcome            original

# `update_role()` cannot set a role to NA, use `remove_role()` for that
if (FALSE) { # \dontrun{
recipe(HHV ~ ., data = biomass) |>
  update_role(sample, new_role = NA_character_)
} # }

# Variables can have more than one role. `add_role()` can be used
# if the column already has at least one role:
recipe(HHV ~ ., data = biomass) |>
  add_role(carbon, sulfur, new_role = "something") |>
  summary()
#> # A tibble: 10 × 4
#>    variable type      role      source  
#>    <chr>    <list>    <chr>     <chr>   
#>  1 sample   <chr [3]> predictor original
#>  2 dataset  <chr [3]> predictor original
#>  3 carbon   <chr [2]> predictor original
#>  4 carbon   <chr [2]> something original
#>  5 hydrogen <chr [2]> predictor original
#>  6 oxygen   <chr [2]> predictor original
#>  7 nitrogen <chr [2]> predictor original
#>  8 sulfur   <chr [2]> predictor original
#>  9 sulfur   <chr [2]> something original
#> 10 HHV      <chr [2]> outcome   original

# `update_role()` has an argument called `old_role` that is required to
# unambiguously update a role when the column currently has multiple roles.
recipe(HHV ~ ., data = biomass) |>
  add_role(carbon, new_role = "something") |>
  update_role(carbon, new_role = "something else", old_role = "something") |>
  summary()
#> # A tibble: 9 × 4
#>   variable type      role           source  
#>   <chr>    <list>    <chr>          <chr>   
#> 1 sample   <chr [3]> predictor      original
#> 2 dataset  <chr [3]> predictor      original
#> 3 carbon   <chr [2]> predictor      original
#> 4 carbon   <chr [2]> something else original
#> 5 hydrogen <chr [2]> predictor      original
#> 6 oxygen   <chr [2]> predictor      original
#> 7 nitrogen <chr [2]> predictor      original
#> 8 sulfur   <chr [2]> predictor      original
#> 9 HHV      <chr [2]> outcome        original

# `carbon` has two roles at the end, so the last `update_role()` fails since
# `old_role` was not given.
if (FALSE) { # \dontrun{
recipe(HHV ~ ., data = biomass) |>
  add_role(carbon, sulfur, new_role = "something") |>
  update_role(carbon, new_role = "something else")
} # }

# To remove a role, `remove_role()` can be used to remove a single role.
recipe(HHV ~ ., data = biomass) |>
  add_role(carbon, new_role = "something") |>
  remove_role(carbon, old_role = "something") |>
  summary()
#> # A tibble: 8 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 sample   <chr [3]> predictor original
#> 2 dataset  <chr [3]> predictor original
#> 3 carbon   <chr [2]> predictor original
#> 4 hydrogen <chr [2]> predictor original
#> 5 oxygen   <chr [2]> predictor original
#> 6 nitrogen <chr [2]> predictor original
#> 7 sulfur   <chr [2]> predictor original
#> 8 HHV      <chr [2]> outcome   original

# To remove all roles, call `remove_role()` multiple times to reset to `NA`
recipe(HHV ~ ., data = biomass) |>
  add_role(carbon, new_role = "something") |>
  remove_role(carbon, old_role = "something") |>
  remove_role(carbon, old_role = "predictor") |>
  summary()
#> # A tibble: 8 × 4
#>   variable type      role      source  
#>   <chr>    <list>    <chr>     <chr>   
#> 1 sample   <chr [3]> predictor original
#> 2 dataset  <chr [3]> predictor original
#> 3 carbon   <chr [2]> NA        original
#> 4 hydrogen <chr [2]> predictor original
#> 5 oxygen   <chr [2]> predictor original
#> 6 nitrogen <chr [2]> predictor original
#> 7 sulfur   <chr [2]> predictor original
#> 8 HHV      <chr [2]> outcome   original

# If the formula method is not used, all columns have a missing role:
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
