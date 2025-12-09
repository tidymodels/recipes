# Convert strings to factors

`step_string2factor()` will convert one or more character vectors to
factors (ordered or unordered).

*Use this step only in special cases* (see Details) and instead convert
strings to factors before using any tidymodels functions.

## Usage

``` r
step_string2factor(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  levels = NULL,
  ordered = FALSE,
  skip = FALSE,
  id = rand_id("string2factor")
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

- levels:

  An optional specification of the levels to be used for the new factor.
  If left `NULL`, the sorted unique values present when `bake` is called
  will be used.

- ordered:

  A single logical value; should the factor(s) be ordered?

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

### When should you use this step?

In most cases, if you are planning to use `step_string2factor()` without
setting `levels`, you will be better off converting those character
variables to factor variables **before using a recipe**.

This can be done using dplyr with the following code

    df <- mutate(df, across(where(is.character), as.factor))

During resampling, the complete set of values might not be in the
character data. Converting them to factors with `step_string2factor()`
then will misconfigure the levels.

If the `levels` argument to `step_string2factor()`is used, it will
convert all variables affected by this step to have the same levels.
Because of this, you will need to know the full set of level when you
define the recipe.

Also, note that
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) has an
option `strings_as_factors` that defaults to `TRUE`. This should be
changed so that raw character data will be applied to
`step_string2factor()`. However, this step can also take existing
factors (but will leave them as-is).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `ordered` , and
`id`:

- terms:

  character, the selectors or variables selected

- ordered:

  logical, are factors ordered

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
[`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md),
[`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md),
[`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md),
[`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
[`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
[`step_integer()`](https://recipes.tidymodels.org/dev/reference/step_integer.md),
[`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md),
[`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md),
[`step_ordinalscore()`](https://recipes.tidymodels.org/dev/reference/step_ordinalscore.md),
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md),
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data(Sacramento, package = "modeldata")

# convert factor to string to demonstrate
Sacramento$city <- as.character(Sacramento$city)

rec <- recipe(~ city + zip, data = Sacramento)

make_factor <- rec |>
  step_string2factor(city)

make_factor <- prep(make_factor,
  training = Sacramento
)

make_factor
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> predictor: 2
#> 
#> ── Training information 
#> Training data contained 932 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Factor variables from: city | Trained

# note that `city` is a factor in recipe output
bake(make_factor, new_data = NULL) |> head()
#> # A tibble: 6 × 2
#>   city       zip   
#>   <fct>      <fct> 
#> 1 SACRAMENTO z95838
#> 2 SACRAMENTO z95823
#> 3 SACRAMENTO z95815
#> 4 SACRAMENTO z95815
#> 5 SACRAMENTO z95824
#> 6 SACRAMENTO z95841

# ...but remains a string in the data
Sacramento |> head()
#> # A tibble: 6 × 9
#>   city       zip     beds baths  sqft type     price latitude longitude
#>   <chr>      <fct>  <int> <dbl> <int> <fct>    <int>    <dbl>     <dbl>
#> 1 SACRAMENTO z95838     2     1   836 Residen… 59222     38.6     -121.
#> 2 SACRAMENTO z95823     3     1  1167 Residen… 68212     38.5     -121.
#> 3 SACRAMENTO z95815     2     1   796 Residen… 68880     38.6     -121.
#> 4 SACRAMENTO z95815     2     1   852 Residen… 69307     38.6     -121.
#> 5 SACRAMENTO z95824     2     1   797 Residen… 81900     38.5     -121.
#> 6 SACRAMENTO z95841     3     1  1122 Condo    89921     38.7     -121.
```
