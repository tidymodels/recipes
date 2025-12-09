# Detect if a particular step or check is used in a recipe

Detect if a particular step or check is used in a recipe

## Usage

``` r
detect_step(recipe, name)
```

## Arguments

- recipe:

  A recipe to check.

- name:

  Character name of a step or check, omitted the prefix. That is, to
  check if `step_intercept` is present, use `name = intercept`.

## Value

Logical indicating if recipes contains given step.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
rec <- recipe(Species ~ ., data = iris) |>
  step_intercept()

detect_step(rec, "intercept")
#> [1] TRUE
```
