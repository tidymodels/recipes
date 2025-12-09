# Check to see if a recipe is trained/prepared

Check to see if a recipe is trained/prepared

## Usage

``` r
fully_trained(x)
```

## Arguments

- x:

  A recipe

## Value

A logical which is true if all of the recipe steps have been run through
`prep`. If no steps have been added to the recipe, `TRUE` is returned
only if the recipe has been prepped.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
rec <- recipe(Species ~ ., data = iris) |>
  step_center(all_numeric())

rec |> fully_trained()
#> [1] FALSE


rec |>
  prep(training = iris) |>
  fully_trained()
#> [1] TRUE
```
