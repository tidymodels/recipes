# Update a recipe step

This `step` method for [`update()`](https://rdrr.io/r/stats/update.html)
takes named arguments as `...` who's values will replace the elements of
the same name in the actual step.

## Usage

``` r
# S3 method for class 'step'
update(object, ...)
```

## Arguments

- object:

  A recipe `step`.

- ...:

  Key-value pairs where the keys match up with names of elements in the
  step, and the values are the new values to update the step with.

## Details

For a step to be updated, it must not already have been trained.
Otherwise, conflicting information can arise between the data returned
from `bake(object, new_data = NULL)` and the information in the step.

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

# Create a recipe using step_bs() with degree = 3
rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
) |>
  step_bs(carbon, hydrogen, degree = 3)

# Update the step to use degree = 4
rec2 <- rec
rec2$steps[[1]] <- update(rec2$steps[[1]], degree = 4)

# Prep both recipes
rec_prepped <- prep(rec, training = biomass_tr)
rec2_prepped <- prep(rec2, training = biomass_tr)

# To see what changed
bake(rec_prepped, new_data = NULL)
#> # A tibble: 456 × 10
#>    oxygen nitrogen sulfur   HHV carbon_bs_1 carbon_bs_2 carbon_bs_3
#>     <dbl>    <dbl>  <dbl> <dbl>       <dbl>       <dbl>       <dbl>
#>  1   42.9     0.41   0     20.0       0.421       0.313      0.0775
#>  2   41.3     0.2    0     19.2       0.423       0.309      0.0754
#>  3   46.2     0.11   0.02  18.3       0.431       0.290      0.0651
#>  4   35.6     3.3    0.16  18.2       0.441       0.258      0.0504
#>  5   40.7     1      0.02  18.4       0.436       0.278      0.0590
#>  6   40.2     2.04   0.1   18.5       0.440       0.262      0.0519
#>  7   38.2     2.68   0.2   18.7       0.434       0.283      0.0613
#>  8   39.7     1.7    0.2   18.3       0.439       0.265      0.0534
#>  9   40.9     0.8    0     18.6       0.426       0.301      0.0710
#> 10   40       1.2    0.1   18.9       0.434       0.282      0.0609
#> # ℹ 446 more rows
#> # ℹ 3 more variables: hydrogen_bs_1 <dbl>, hydrogen_bs_2 <dbl>,
#> #   hydrogen_bs_3 <dbl>
bake(rec2_prepped, new_data = NULL)
#> # A tibble: 456 × 12
#>    oxygen nitrogen sulfur   HHV carbon_bs_1 carbon_bs_2 carbon_bs_3
#>     <dbl>    <dbl>  <dbl> <dbl>       <dbl>       <dbl>       <dbl>
#>  1   42.9     0.41   0     20.0       0.322       0.359       0.178
#>  2   41.3     0.2    0     19.2       0.325       0.357       0.174
#>  3   46.2     0.11   0.02  18.3       0.344       0.347       0.156
#>  4   35.6     3.3    0.16  18.2       0.371       0.325       0.127
#>  5   40.7     1      0.02  18.4       0.355       0.339       0.144
#>  6   40.2     2.04   0.1   18.5       0.368       0.328       0.130
#>  7   38.2     2.68   0.2   18.7       0.350       0.342       0.149
#>  8   39.7     1.7    0.2   18.3       0.365       0.331       0.133
#>  9   40.9     0.8    0     18.6       0.333       0.353       0.166
#> 10   40       1.2    0.1   18.9       0.351       0.342       0.148
#> # ℹ 446 more rows
#> # ℹ 5 more variables: carbon_bs_4 <dbl>, hydrogen_bs_1 <dbl>,
#> #   hydrogen_bs_2 <dbl>, hydrogen_bs_3 <dbl>, hydrogen_bs_4 <dbl>

# Cannot update a recipe step that has been trained!
if (FALSE) { # \dontrun{
update(rec_prepped$steps[[1]], degree = 4)
} # }
```
