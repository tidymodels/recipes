# Update role specific requirements

`update_role_requirements()` allows you to fine tune requirements of the
various roles you might come across in recipes (see
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
for general information about roles). Role requirements can only be
altered for roles that exist in the *original* data supplied to
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
they are not applied to columns computed by steps.

Like
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
`update_role_requirements()` is applied to the recipe *immediately*,
unlike the `step_*()` functions which do most of their work at
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) time.

## Usage

``` r
update_role_requirements(recipe, role, ..., bake = NULL)
```

## Arguments

- recipe:

  A recipe.

- role:

  A string representing the role that you'd like to modify the
  requirements of. This must be a role that already exists in the
  recipe.

- ...:

  These dots are for future extensions and must be empty.

- bake:

  At [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
  time, should a check be done to ensure that all columns of this role
  that were supplied to
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  also be present in the `new_data` supplied to
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?

  Must be a single `TRUE` or `FALSE`. The default, `NULL`, won't modify
  this requirement.

  The following represents the default bake time requirements of
  specific types of roles:

  - `"outcome"`: Not required at bake time. Can't be changed.

  - `"predictor"`: Required at bake time. Can't be changed.

  - `"case_weights"`: Not required at bake time by default.

  - `NA`: Required at bake time by default.

  - Custom roles: Required at bake time by default.

## Examples

``` r
df <- tibble(y = c(1, 2, 3), x = c(4, 5, 6), var = c("a", "b", "c"))

# Let's assume that you have a `var` column that isn't used in the recipe.
# We typically recommend that you remove this column before passing the
# `data` to `recipe()`, but for now let's pass it through and assign it an
# `"id"` role.
rec <- recipe(y ~ ., df) |>
  update_role(var, new_role = "id") |>
  step_center(x)

prepped <- prep(rec, df)

# Now assume you have some "new data" and you are ready to `bake()` it
# to prepare it for prediction purposes. Here, you might not have `var`
# available as a column because it isn't important to your model.
new_data <- df[c("y", "x")]

# By default `var` is required at `bake()` time because we don't know if
# you actually use it in the recipe or not
try(bake(prepped, new_data))
#> Error in bake(prepped, new_data) : 
#>   ✖ The following required columns are missing from `new_data`:
#>   `var`.
#> ℹ These columns have one of the following roles, which are required at
#>   `bake()` time: `id`.
#> ℹ If these roles are not required at `bake()` time, use
#>   `update_role_requirements(role = "your_role", bake = FALSE)`.

# You can turn off this check by using `update_role_requirements()` and
# setting `bake = FALSE` for the `"id"` role. We recommend doing this on
# the original unprepped recipe, but it will also work on a prepped recipe.
rec <- update_role_requirements(rec, "id", bake = FALSE)
prepped <- prep(rec, df)

# Now you can `bake()` on `new_data` even though `var` is missing
bake(prepped, new_data)
#> # A tibble: 3 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1    -1     1
#> 2     0     2
#> 3     1     3
```
