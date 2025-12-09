# Add a New Operation to the Current Recipe

`add_step()` adds a step to the last location in the recipe.
`add_check()` does the same for checks.

## Usage

``` r
add_step(rec, object)

add_check(rec, object)
```

## Arguments

- rec:

  A
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).

- object:

  A step or check object.

## Value

A updated
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
with the new operation in the last slot.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
