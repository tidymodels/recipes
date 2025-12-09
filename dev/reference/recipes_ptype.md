# Prototype of recipe object

This helper function returns the prototype of the input data set
expected by the recipe object.

## Usage

``` r
recipes_ptype(x, ..., stage = "prep")
```

## Arguments

- x:

  A `recipe` object.

- ...:

  currently not used.

- stage:

  A single character. Must be one of `"prep"` or `"bake"`. See details
  for more. Defaults to `"prep"`.

## Value

A zero row tibble.

## Details

The returned ptype is a tibble of the data set that the recipe object is
expecting. The specifics of which columns depend on the `stage`.

At [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
time, when `stage = "prep"`, the ptype is the data passed to
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).
The following code chunk represents a possible recipe scenario.
`recipes_ptype(rec_spec, stage = "prep")` and
`recipes_ptype(rec_prep, stage = "prep")` both return a ptype tibble
corresponding to `data_ptype`. This information is used internally in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) to
verify that `data_training` has the right columns with the right types.

    rec_spec <- recipe(outcome ~ ., data = data_ptype) |>
      step_normalize(all_numeric_predictors()) |>
      step_dummy(all_nominal_predictors())

    rec_prep <- prep(rec_spec, training = data_training)

At [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
time, when `stage = "bake"`, the ptype represents the data that are
required for
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) to run.

    data_bake <- bake(rec_prep, new_data = data_testing)

What this means in practice is that unless otherwise specified,
everything but outcomes and case weights are required. These
requirements can be changed with
[`update_role_requirements()`](https://recipes.tidymodels.org/dev/reference/update_role_requirements.md),
and `recipes_ptype()` respects those changes.

`recipes_ptype()` returns `NULL` on recipes created prior to version
1.1.0.

Note that the order of the columns aren't guaranteed to align with
`data_ptype` as the data internally is ordered according to roles.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
[recipes_ptype_validate](https://recipes.tidymodels.org/dev/reference/recipes_ptype_validate.md)

## Examples

``` r
training <- tibble(
  y = 1:10,
  id = 1:10,
  x1 = letters[1:10],
  x2 = factor(letters[1:10]),
  cw = hardhat::importance_weights(1:10)
)
training
#> # A tibble: 10 × 5
#>        y    id x1    x2           cw
#>    <int> <int> <chr> <fct> <imp_wts>
#>  1     1     1 a     a             1
#>  2     2     2 b     b             2
#>  3     3     3 c     c             3
#>  4     4     4 d     d             4
#>  5     5     5 e     e             5
#>  6     6     6 f     f             6
#>  7     7     7 g     g             7
#>  8     8     8 h     h             8
#>  9     9     9 i     i             9
#> 10    10    10 j     j            10

rec_spec <- recipe(y ~ ., data = training)

# outcomes and case_weights are not required at bake time
recipes_ptype(rec_spec, stage = "prep")
#> # A tibble: 0 × 5
#> # ℹ 5 variables: id <int>, x1 <chr>, x2 <fct>, cw <imp_wts>, y <int>
recipes_ptype(rec_spec, stage = "bake")
#> # A tibble: 0 × 3
#> # ℹ 3 variables: id <int>, x1 <chr>, x2 <fct>

rec_spec <- recipe(y ~ ., data = training) |>
  update_role(x1, new_role = "id")

# outcomes and case_weights are not required at bake time
# "id" column is assumed to be needed
recipes_ptype(rec_spec, stage = "prep")
#> # A tibble: 0 × 5
#> # ℹ 5 variables: id <int>, x1 <chr>, x2 <fct>, cw <imp_wts>, y <int>
recipes_ptype(rec_spec, stage = "bake")
#> # A tibble: 0 × 3
#> # ℹ 3 variables: id <int>, x1 <chr>, x2 <fct>

rec_spec <- recipe(y ~ ., data = training) |>
  update_role(x1, new_role = "id") |>
  update_role_requirements("id", bake = FALSE)

# update_role_requirements() is used to specify that "id" isn't needed
recipes_ptype(rec_spec, stage = "prep")
#> # A tibble: 0 × 5
#> # ℹ 5 variables: id <int>, x1 <chr>, x2 <fct>, cw <imp_wts>, y <int>
recipes_ptype(rec_spec, stage = "bake")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: id <int>, x2 <fct>
```
