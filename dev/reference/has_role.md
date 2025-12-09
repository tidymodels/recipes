# Role Selection

`has_role()`, `all_predictors()`, and `all_outcomes()` can be used to
select variables in a formula that have certain roles.

**In most cases**, the right approach for users will be use to use the
predictor-specific selectors such as `all_numeric_predictors()` and
`all_nominal_predictors()`. In general you should be careful about using
`-all_outcomes()` if a `*_predictors()` selector would do what you want.

Similarly, `has_type()`, `all_numeric()`, `all_integer()`,
`all_double()`, `all_nominal()`, `all_ordered()`, `all_unordered()`,
`all_factor()`, `all_string()`, `all_date()` and `all_datetime()` are
used to select columns based on their data type.

`all_factor()` captures ordered and unordered factors, `all_string()`
captures characters, `all_unordered()` captures unordered factors and
characters, `all_ordered()` captures ordered factors, `all_nominal()`
captures characters, unordered and ordered factors.

`all_integer()` captures integers, `all_double()` captures doubles,
`all_numeric()` captures all kinds of numeric.

`all_date()` captures [`Date()`](https://rdrr.io/r/base/Dates.html)
variables, `all_datetime()` captures
[`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html) variables.

See
[selections](https://recipes.tidymodels.org/dev/reference/selections.md)
for more details.

`current_info()` is an internal function.

All of these functions have have limited utility outside of column
selection in step functions.

## Usage

``` r
has_role(match = "predictor")

has_type(match = "numeric")

all_outcomes()

all_predictors()

all_date()

all_date_predictors()

all_datetime()

all_datetime_predictors()

all_double()

all_double_predictors()

all_factor()

all_factor_predictors()

all_integer()

all_integer_predictors()

all_logical()

all_logical_predictors()

all_nominal()

all_nominal_predictors()

all_numeric()

all_numeric_predictors()

all_ordered()

all_ordered_predictors()

all_string()

all_string_predictors()

all_unordered()

all_unordered_predictors()

current_info()
```

## Arguments

- match:

  A single character string for the query. Exact matching is used (i.e.
  regular expressions won't work).

## Value

Selector functions return an integer vector.

`current_info()` returns an environment with objects `vars` and `data`.

## Examples

``` r
data(biomass, package = "modeldata")

rec <- recipe(biomass) |>
  update_role(
    carbon, hydrogen, oxygen, nitrogen, sulfur,
    new_role = "predictor"
  ) |>
  update_role(HHV, new_role = "outcome") |>
  update_role(sample, new_role = "id variable") |>
  update_role(dataset, new_role = "splitting indicator")

recipe_info <- summary(rec)
recipe_info
#> # A tibble: 8 × 4
#>   variable type      role                source  
#>   <chr>    <list>    <chr>               <chr>   
#> 1 sample   <chr [3]> id variable         original
#> 2 dataset  <chr [3]> splitting indicator original
#> 3 carbon   <chr [2]> predictor           original
#> 4 hydrogen <chr [2]> predictor           original
#> 5 oxygen   <chr [2]> predictor           original
#> 6 nitrogen <chr [2]> predictor           original
#> 7 sulfur   <chr [2]> predictor           original
#> 8 HHV      <chr [2]> outcome             original

# Centering on all predictors except carbon
rec |>
  step_center(all_predictors(), -carbon) |>
  prep(training = biomass) |>
  bake(new_data = NULL)
#> # A tibble: 536 × 8
#>    sample         dataset carbon hydrogen oxygen nitrogen  sulfur   HHV
#>    <chr>          <chr>    <dbl>    <dbl>  <dbl>    <dbl>   <dbl> <dbl>
#>  1 Akhrot Shell   Traini…   49.8   0.181   4.37   -0.667  -0.234   20.0
#>  2 Alabama Oak W… Traini…   49.5   0.241   2.73   -0.877  -0.234   19.2
#>  3 Alder          Traini…   47.8   0.341   7.68   -0.967  -0.214   18.3
#>  4 Alfalfa        Traini…   45.1  -0.489  -2.97    2.22   -0.0736  18.2
#>  5 Alfalfa Seed … Traini…   46.8  -0.0586  2.15   -0.0772 -0.214   18.4
#>  6 Alfalfa Stalks Traini…   45.4   0.291   1.63    0.963  -0.134   18.5
#>  7 Alfalfa Stems  Traini…   47.2   0.531  -0.383   1.60   -0.0336  18.7
#>  8 Alfalfa Straw  Traini…   45.7   0.241   1.13    0.623  -0.0336  18.3
#>  9 Almond         Traini…   48.8   0.0414  2.33   -0.277  -0.234   18.6
#> 10 Almond Hull    Traini…   47.1   0.441   1.43    0.123  -0.134   18.9
#> # ℹ 526 more rows
```
