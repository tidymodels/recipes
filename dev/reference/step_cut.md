# Cut a numeric variable into a factor

`step_cut()` creates a *specification* of a recipe step that cuts a
numeric variable into a factor based on provided boundary values.

## Usage

``` r
step_cut(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  breaks,
  include_outside_range = FALSE,
  skip = FALSE,
  id = rand_id("cut")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- breaks:

  A numeric vector with at least one cut point.

- include_outside_range:

  Logical, indicating if values outside the range in the train set
  should be included in the lowest or highest bucket. Defaults to
  `FALSE`, values outside the original range will be set to `NA`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Unlike the [`base::cut()`](https://rdrr.io/r/base/cut.html) function
there is no need to specify the min and the max values in the breaks.
All values before the lowest break point will end up in the first
bucket, all values after the last break points will end up in the last.

`step_cut()` will call [`base::cut()`](https://rdrr.io/r/base/cut.html)
in the baking step with `include.lowest` set to `TRUE`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, the location of the cuts

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other discretization steps:
[`step_discretize()`](https://recipes.tidymodels.org/dev/reference/step_discretize.md)

## Examples

``` r
df <- data.frame(x = 1:10, y = 5:14)
rec <- recipe(df)

# The min and max of the variable are used as boundaries
# if they exceed the breaks
rec |>
  step_cut(x, breaks = 5) |>
  prep() |>
  bake(df)
#> # A tibble: 10 × 2
#>    x          y
#>    <fct>  <int>
#>  1 [1,5]      5
#>  2 [1,5]      6
#>  3 [1,5]      7
#>  4 [1,5]      8
#>  5 [1,5]      9
#>  6 (5,10]    10
#>  7 (5,10]    11
#>  8 (5,10]    12
#>  9 (5,10]    13
#> 10 (5,10]    14

# You can use the same breaks on multiple variables
# then for each variable the boundaries are set separately
rec |>
  step_cut(x, y, breaks = c(6, 9)) |>
  prep() |>
  bake(df)
#> # A tibble: 10 × 2
#>    x      y     
#>    <fct>  <fct> 
#>  1 [1,6]  [5,6] 
#>  2 [1,6]  [5,6] 
#>  3 [1,6]  (6,9] 
#>  4 [1,6]  (6,9] 
#>  5 [1,6]  (6,9] 
#>  6 [1,6]  (9,14]
#>  7 (6,9]  (9,14]
#>  8 (6,9]  (9,14]
#>  9 (6,9]  (9,14]
#> 10 (9,10] (9,14]

# You can keep the original variables using `step_mutate` or
# `step_mutate_at`, for transforming multiple variables at once
rec |>
  step_mutate(x_orig = x) |>
  step_cut(x, breaks = 5) |>
  prep() |>
  bake(df)
#> # A tibble: 10 × 3
#>    x          y x_orig
#>    <fct>  <int>  <int>
#>  1 [1,5]      5      1
#>  2 [1,5]      6      2
#>  3 [1,5]      7      3
#>  4 [1,5]      8      4
#>  5 [1,5]      9      5
#>  6 (5,10]    10      6
#>  7 (5,10]    11      7
#>  8 (5,10]    12      8
#>  9 (5,10]    13      9
#> 10 (5,10]    14     10

# It is up to you if you want values outside the
# range learned at prep to be included
new_df <- data.frame(x = 1:11, y = 5:15)
rec |>
  step_cut(x, breaks = 5, include_outside_range = TRUE) |>
  prep() |>
  bake(new_df)
#> # A tibble: 11 × 2
#>    x           y
#>    <fct>   <int>
#>  1 [min,5]     5
#>  2 [min,5]     6
#>  3 [min,5]     7
#>  4 [min,5]     8
#>  5 [min,5]     9
#>  6 (5,max]    10
#>  7 (5,max]    11
#>  8 (5,max]    12
#>  9 (5,max]    13
#> 10 (5,max]    14
#> 11 (5,max]    15

rec |>
  step_cut(x, breaks = 5, include_outside_range = FALSE) |>
  prep() |>
  bake(new_df)
#> # A tibble: 11 × 2
#>    x          y
#>    <fct>  <int>
#>  1 [1,5]      5
#>  2 [1,5]      6
#>  3 [1,5]      7
#>  4 [1,5]      8
#>  5 [1,5]      9
#>  6 (5,10]    10
#>  7 (5,10]    11
#>  8 (5,10]    12
#>  9 (5,10]    13
#> 10 (5,10]    14
#> 11 NA        15
```
