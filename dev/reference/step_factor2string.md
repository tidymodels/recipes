# Convert factors to strings

`step_factor2string()` creates a *specification* of a recipe step that
will convert one or more factor vectors to strings.

## Usage

``` r
step_factor2string(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = FALSE,
  skip = FALSE,
  id = rand_id("factor2string")
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

## Details

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md) has
an option `strings_as_factors` that defaults to `TRUE`. If this step is
used with the default option, the strings produced by this step will not
be converted to factors.

Remember that categorical data that will be directly passed to a model
should be encoded as factors. This step is helpful for ancillary columns
(such as identifiers) that will not be computed on in the model.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

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
[`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
[`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
[`step_integer()`](https://recipes.tidymodels.org/dev/reference/step_integer.md),
[`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md),
[`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md),
[`step_ordinalscore()`](https://recipes.tidymodels.org/dev/reference/step_ordinalscore.md),
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md),
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data(Sacramento, package = "modeldata")

rec <- recipe(~ city + zip, data = Sacramento)

make_string <- rec |>
  step_factor2string(city)

make_string <- prep(make_string,
  training = Sacramento,
  strings_as_factors = FALSE
)

make_string
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
#> • Character variables from: city | Trained

# note that `city` is a string in recipe output
bake(make_string, new_data = NULL) |> head()
#> # A tibble: 6 × 2
#>   city       zip   
#>   <chr>      <fct> 
#> 1 SACRAMENTO z95838
#> 2 SACRAMENTO z95823
#> 3 SACRAMENTO z95815
#> 4 SACRAMENTO z95815
#> 5 SACRAMENTO z95824
#> 6 SACRAMENTO z95841

# ...but remains a factor in the original data
Sacramento |> head()
#> # A tibble: 6 × 9
#>   city       zip     beds baths  sqft type     price latitude longitude
#>   <fct>      <fct>  <int> <dbl> <int> <fct>    <int>    <dbl>     <dbl>
#> 1 SACRAMENTO z95838     2     1   836 Residen… 59222     38.6     -121.
#> 2 SACRAMENTO z95823     3     1  1167 Residen… 68212     38.5     -121.
#> 3 SACRAMENTO z95815     2     1   796 Residen… 68880     38.6     -121.
#> 4 SACRAMENTO z95815     2     1   852 Residen… 69307     38.6     -121.
#> 5 SACRAMENTO z95824     2     1   797 Residen… 81900     38.5     -121.
#> 6 SACRAMENTO z95841     3     1  1122 Condo    89921     38.7     -121.
```
