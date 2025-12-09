# Shuffle variables

`step_shuffle()` creates a *specification* of a recipe step that will
randomly change the order of rows for selected variables.

## Usage

``` r
step_shuffle(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("shuffle")
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

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
integers <- data.frame(A = 1:12, B = 13:24, C = 25:36)

library(dplyr)
rec <- recipe(~ A + B + C, data = integers) |>
  step_shuffle(A, B)

rand_set <- prep(rec, training = integers)

set.seed(5377)
bake(rand_set, integers)
#> # A tibble: 12 × 3
#>        A     B     C
#>    <int> <int> <int>
#>  1     5    13    25
#>  2     4    24    26
#>  3     9    23    27
#>  4     1    18    28
#>  5    11    14    29
#>  6    10    21    30
#>  7     6    19    31
#>  8     3    22    32
#>  9    12    16    33
#> 10     7    15    34
#> 11     2    17    35
#> 12     8    20    36

tidy(rec, number = 1)
#> # A tibble: 2 × 2
#>   terms id           
#>   <chr> <chr>        
#> 1 A     shuffle_onc2n
#> 2 B     shuffle_onc2n
tidy(rand_set, number = 1)
#> # A tibble: 2 × 2
#>   terms id           
#>   <chr> <chr>        
#> 1 A     shuffle_onc2n
#> 2 B     shuffle_onc2n
```
