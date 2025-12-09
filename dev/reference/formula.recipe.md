# Create a formula from a prepared recipe

In case a model formula is required, the formula method can be used on a
recipe to show what predictors and outcome(s) could be used.

## Usage

``` r
# S3 method for class 'recipe'
formula(x, ...)
```

## Arguments

- x:

  A recipe object that has been prepared.

- ...:

  Note currently used.

## Value

A formula.

## Examples

``` r
formula(recipe(Species + Sepal.Length ~ ., data = iris) |> prep())
#> Species + Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
#> <environment: 0x55ff03138df0>

iris_rec <- recipe(Species ~ ., data = iris) |>
  step_center(all_numeric()) |>
  prep()
formula(iris_rec)
#> Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
#> <environment: 0x55ff03ab2c20>
```
