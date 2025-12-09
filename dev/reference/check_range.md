# Check range consistency

`check_range()` creates a *specification* of a recipe check that will
check if the range of a numeric variable changed in the new data.

## Usage

``` r
check_range(
  recipe,
  ...,
  role = NA,
  skip = FALSE,
  trained = FALSE,
  slack_prop = 0.05,
  warn = FALSE,
  lower = NULL,
  upper = NULL,
  id = rand_id("range_check_")
)
```

## Arguments

- recipe:

  A recipe object. The check will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this check. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this check since no new variables are created.

- skip:

  A logical. Should the check be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- trained:

  A logical for whether the selectors in `...` have been resolved by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- slack_prop:

  The allowed slack as a proportion of the range of the variable in the
  train set.

- warn:

  If `TRUE` the check will throw a warning instead of an error when
  failing.

- lower:

  A named numeric vector of minimum values in the train set. This is
  `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- upper:

  A named numeric vector of maximum values in the train set. This is
  `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- id:

  A character string that is unique to this check to identify it.

## Value

An updated version of `recipe` with the new check added to the sequence
of any existing operations.

## Details

The amount of slack that is allowed is determined by the `slack_prop`.
This is a numeric of length one or two. If of length one, the same
proportion will be used at both ends of the train set range. If of
length two, its first value is used to compute the allowed slack at the
lower end, the second to compute the allowed slack at the upper end.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this check, a tibble with columns `terms` (the selectors or variables
selected) and `value` (the means) is returned.

## See also

Other checks:
[`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md),
[`check_cols()`](https://recipes.tidymodels.org/dev/reference/check_cols.md),
[`check_missing()`](https://recipes.tidymodels.org/dev/reference/check_missing.md),
[`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md)

## Examples

``` r
slack_df <- data_frame(x = 0:100)
#> Warning: `data_frame()` was deprecated in tibble 1.1.0.
#> ℹ Please use `tibble()` instead.
slack_new_data <- data_frame(x = -10:110)

# this will fail the check both ends
if (FALSE) { # \dontrun{
recipe(slack_df) |>
  check_range(x) |>
  prep() |>
  bake(slack_new_data)
} # }

# this will fail the check only at the upper end
if (FALSE) { # \dontrun{
recipe(slack_df) |>
  check_range(x, slack_prop = c(0.1, 0.05)) |>
  prep() |>
  bake(slack_new_data)
} # }

# give a warning instead of an error
if (FALSE) { # \dontrun{
recipe(slack_df) |>
  check_range(x, warn = TRUE) |>
  prep() |>
  bake(slack_new_data)
} # }
```
