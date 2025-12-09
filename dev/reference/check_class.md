# Check variable class

`check_class` creates a *specification* of a recipe check that will
check if a variable is of a designated class.

## Usage

``` r
check_class(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  class_nm = NULL,
  allow_additional = FALSE,
  skip = FALSE,
  class_list = NULL,
  id = rand_id("class")
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

- class_nm:

  A character vector that will be used in `inherits` to check the class.
  If `NULL` the classes will be learned in `prep`. Can contain more than
  one class.

- allow_additional:

  If `TRUE` a variable is allowed to have additional classes to the
  one(s) that are checked.

- skip:

  A logical. Should the check be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- class_list:

  A named list of column classes. This is `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- id:

  A character string that is unique to this check to identify it.

## Value

An updated version of `recipe` with the new check added to the sequence
of any existing operations.

## Details

This function can check the classes of the variables in two ways. When
the `class` argument is provided it will check if all the variables
specified are of the given class. If this argument is `NULL`, the check
will learn the classes of each of the specified variables in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md). Both
ways will break
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) if the
variables are not of the requested class. If a variable has multiple
classes in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), all
the classes are checked. Please note that in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) the
argument `strings_as_factors` defaults to `TRUE`. If the train set
contains character variables the check will be break
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) when
`strings_as_factors` is `TRUE`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this check, a tibble with columns `terms` (the selectors or variables
selected) and `value` (the type) is returned.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other checks:
[`check_cols()`](https://recipes.tidymodels.org/dev/reference/check_cols.md),
[`check_missing()`](https://recipes.tidymodels.org/dev/reference/check_missing.md),
[`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md),
[`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md)

## Examples

``` r
library(dplyr)
data(Sacramento, package = "modeldata")

# Learn the classes on the train set
train <- Sacramento[1:500, ]
test <- Sacramento[501:nrow(Sacramento), ]
recipe(train, sqft ~ .) |>
  check_class(everything()) |>
  prep(train, strings_as_factors = FALSE) |>
  bake(test)
#> Warning: The `strings_as_factors` argument of `prep.recipe()` is deprecated as
#> of recipes 1.3.0.
#> ℹ Please use the `strings_as_factors` argument of `recipe()` instead.
#> # A tibble: 432 × 9
#>    city         zip    beds baths type   price latitude longitude  sqft
#>    <fct>        <fct> <int> <dbl> <fct>  <int>    <dbl>     <dbl> <int>
#>  1 SACRAMENTO   z958…     4   2   Resi… 328578     38.6     -122.  1659
#>  2 ELK_GROVE    z957…     3   3   Resi… 331000     38.4     -121.  2442
#>  3 RANCHO_CORD… z957…     4   3   Resi… 331500     38.6     -121.  2590
#>  4 SACRAMENTO   z958…     4   2   Resi… 340000     38.6     -122.  2155
#>  5 SACRAMENTO   z958…     3   2   Resi… 344755     38.7     -121.  1673
#>  6 SACRAMENTO   z958…     3   2   Resi… 345746     38.5     -121.  1810
#>  7 ELK_GROVE    z957…     4   2   Resi… 351000     38.4     -121.  2789
#>  8 GALT         z956…     4   2   Resi… 353767     38.3     -121.  1606
#>  9 GALT         z956…     5   3.5 Resi… 355000     38.3     -121.  3499
#> 10 SACRAMENTO   z958…     4   2   Resi… 356035     38.7     -122.  2166
#> # ℹ 422 more rows

# Manual specification
recipe(train, sqft ~ .) |>
  check_class(sqft, class_nm = "integer") |>
  check_class(city, zip, type, class_nm = "factor") |>
  check_class(latitude, longitude, class_nm = "numeric") |>
  prep(train, strings_as_factors = FALSE) |>
  bake(test)
#> # A tibble: 432 × 9
#>    city         zip    beds baths type   price latitude longitude  sqft
#>    <fct>        <fct> <int> <dbl> <fct>  <int>    <dbl>     <dbl> <int>
#>  1 SACRAMENTO   z958…     4   2   Resi… 328578     38.6     -122.  1659
#>  2 ELK_GROVE    z957…     3   3   Resi… 331000     38.4     -121.  2442
#>  3 RANCHO_CORD… z957…     4   3   Resi… 331500     38.6     -121.  2590
#>  4 SACRAMENTO   z958…     4   2   Resi… 340000     38.6     -122.  2155
#>  5 SACRAMENTO   z958…     3   2   Resi… 344755     38.7     -121.  1673
#>  6 SACRAMENTO   z958…     3   2   Resi… 345746     38.5     -121.  1810
#>  7 ELK_GROVE    z957…     4   2   Resi… 351000     38.4     -121.  2789
#>  8 GALT         z956…     4   2   Resi… 353767     38.3     -121.  1606
#>  9 GALT         z956…     5   3.5 Resi… 355000     38.3     -121.  3499
#> 10 SACRAMENTO   z958…     4   2   Resi… 356035     38.7     -122.  2166
#> # ℹ 422 more rows

# By default only the classes that are specified
#   are allowed.
x_df <- tibble(time = c(Sys.time() - 60, Sys.time()))
x_df$time |> class()
#> [1] "POSIXct" "POSIXt" 
if (FALSE) { # \dontrun{
recipe(x_df) |>
  check_class(time, class_nm = "POSIXt") |>
  prep(x_df) |>
  bake_(x_df)
} # }

# Use allow_additional = TRUE if you are fine with it
recipe(x_df) |>
  check_class(time, class_nm = "POSIXt", allow_additional = TRUE) |>
  prep(x_df) |>
  bake(x_df)
#> # A tibble: 2 × 1
#>   time               
#>   <dttm>             
#> 1 2025-12-09 19:32:21
#> 2 2025-12-09 19:33:21
```
