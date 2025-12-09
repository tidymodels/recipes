# Check for new values

`check_new_values()` creates a *specification* of a recipe operation
that will check if variables contain new values.

## Usage

``` r
check_new_values(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  ignore_NA = TRUE,
  values = NULL,
  skip = FALSE,
  id = rand_id("new_values")
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

- trained:

  A logical for whether the selectors in `...` have been resolved by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- ignore_NA:

  A logical that indicates if we should consider missing values as value
  or not. Defaults to `TRUE`.

- values:

  A named list with the allowed values. This is `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- skip:

  A logical. Should the check be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this check to identify it.

## Value

An updated version of `recipe` with the new check added to the sequence
of any existing operations.

## Details

This check will break the
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
function if any of the checked columns does contain values it did not
contain when
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) was
called on the recipe. If the check passes, nothing is changed to the
data.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this check, a tibble with columns `terms` (the selectors or variables
selected) is returned.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other checks:
[`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md),
[`check_cols()`](https://recipes.tidymodels.org/dev/reference/check_cols.md),
[`check_missing()`](https://recipes.tidymodels.org/dev/reference/check_missing.md),
[`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md)

## Examples

``` r
data(credit_data, package = "modeldata")

# If the test passes, `new_data` is returned unaltered
recipe(credit_data) |>
  check_new_values(Home) |>
  prep() |>
  bake(new_data = credit_data)
#> # A tibble: 4,454 × 14
#>    Status Seniority Home     Time   Age Marital Records Job    Expenses
#>    <fct>      <int> <fct>   <int> <int> <fct>   <fct>   <fct>     <int>
#>  1 good           9 rent       60    30 married no      freel…       73
#>  2 good          17 rent       60    58 widow   no      fixed        48
#>  3 bad           10 owner      36    46 married yes     freel…       90
#>  4 good           0 rent       60    24 single  no      fixed        63
#>  5 good           0 rent       36    26 single  no      fixed        46
#>  6 good           1 owner      60    36 married no      fixed        75
#>  7 good          29 owner      60    44 married no      fixed        75
#>  8 good           9 parents    12    27 single  no      fixed        35
#>  9 good           0 owner      60    32 married no      freel…       90
#> 10 bad            0 parents    48    41 married no      parti…       90
#> # ℹ 4,444 more rows
#> # ℹ 5 more variables: Income <int>, Assets <int>, Debt <int>,
#> #   Amount <int>, Price <int>

# If `new_data` contains values not in `x` at the [prep()] function,
# the [bake()] function will break.
if (FALSE) { # \dontrun{
recipe(credit_data |> dplyr::filter(Home != "rent")) |>
  check_new_values(Home) |>
  prep() |>
  bake(new_data = credit_data)
} # }

# By default missing values are ignored, so this passes.
recipe(credit_data |> dplyr::filter(!is.na(Home))) |>
  check_new_values(Home) |>
  prep() |>
  bake(credit_data)
#> # A tibble: 4,454 × 14
#>    Status Seniority Home     Time   Age Marital Records Job    Expenses
#>    <fct>      <int> <fct>   <int> <int> <fct>   <fct>   <fct>     <int>
#>  1 good           9 rent       60    30 married no      freel…       73
#>  2 good          17 rent       60    58 widow   no      fixed        48
#>  3 bad           10 owner      36    46 married yes     freel…       90
#>  4 good           0 rent       60    24 single  no      fixed        63
#>  5 good           0 rent       36    26 single  no      fixed        46
#>  6 good           1 owner      60    36 married no      fixed        75
#>  7 good          29 owner      60    44 married no      fixed        75
#>  8 good           9 parents    12    27 single  no      fixed        35
#>  9 good           0 owner      60    32 married no      freel…       90
#> 10 bad            0 parents    48    41 married no      parti…       90
#> # ℹ 4,444 more rows
#> # ℹ 5 more variables: Income <int>, Assets <int>, Debt <int>,
#> #   Amount <int>, Price <int>

# Use `ignore_NA = FALSE` if you consider missing values  as a value,
# that should not occur when not observed in the train set.
if (FALSE) { # \dontrun{
recipe(credit_data |> dplyr::filter(!is.na(Home))) |>
  check_new_values(Home, ignore_NA = FALSE) |>
  prep() |>
  bake(credit_data)
} # }
```
