# Check for missing values

`check_missing()` creates a *specification* of a recipe operation that
will check if variables contain missing values.

## Usage

``` r
check_missing(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("missing")
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
function if any of the checked columns does contain `NA` values. If the
check passes, nothing is changed to the data.

## tidy() results

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this check, a tibble with column `terms` (the selectors or variables
selected) is returned.

## See also

Other checks:
[`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md),
[`check_cols()`](https://recipes.tidymodels.org/dev/reference/check_cols.md),
[`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md),
[`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md)

## Examples

``` r
data(credit_data, package = "modeldata")
is.na(credit_data) |> colSums()
#>    Status Seniority      Home      Time       Age   Marital   Records 
#>         0         0         6         0         0         1         0 
#>       Job  Expenses    Income    Assets      Debt    Amount     Price 
#>         2         0       381        47        18         0         0 

# If the test passes, `new_data` is returned unaltered
recipe(credit_data) |>
  check_missing(Age, Expenses) |>
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

# If your training set doesn't pass, prep() will stop with an error
if (FALSE) { # \dontrun{
recipe(credit_data) |>
  check_missing(Income) |>
  prep()
} # }

# If `new_data` contain missing values, the check will stop `bake()`

train_data <- credit_data |> dplyr::filter(Income > 150)
test_data <- credit_data |> dplyr::filter(Income <= 150 | is.na(Income))

rp <- recipe(train_data) |>
  check_missing(Income) |>
  prep()

bake(rp, train_data)
#> # A tibble: 1,338 × 14
#>    Status Seniority Home   Time   Age Marital Records Job      Expenses
#>    <fct>      <int> <fct> <int> <int> <fct>   <fct>   <fct>       <int>
#>  1 bad           10 owner    36    46 married yes     freelan…       90
#>  2 good           0 rent     60    24 single  no      fixed          63
#>  3 good           1 owner    60    36 married no      fixed          75
#>  4 good           8 owner    60    30 married no      fixed          75
#>  5 good          19 priv     36    37 married no      fixed          75
#>  6 good          15 priv     24    52 single  no      freelan…       35
#>  7 good          33 rent     24    68 married no      freelan…       65
#>  8 good           5 owner    60    22 single  no      fixed          45
#>  9 good          19 owner    60    43 single  no      fixed          75
#> 10 good          15 owner    36    43 married no      fixed          75
#> # ℹ 1,328 more rows
#> # ℹ 5 more variables: Income <int>, Assets <int>, Debt <int>,
#> #   Amount <int>, Price <int>
if (FALSE) { # \dontrun{
bake(rp, test_data)
} # }
```
