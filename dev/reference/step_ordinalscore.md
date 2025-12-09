# Convert ordinal factors to numeric scores

`step_ordinalscore()` creates a *specification* of a recipe step that
will convert ordinal factor variables into numeric scores.

## Usage

``` r
step_ordinalscore(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  convert = as.numeric,
  skip = FALSE,
  id = rand_id("ordinalscore")
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

- convert:

  A function that takes an ordinal factor vector as an input and outputs
  a single numeric variable.

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

Dummy variables from ordered factors with `C` levels will create
polynomial basis functions with `C-1` terms. As an alternative, this
step can be used to translate the ordered levels into a single numeric
vector of values that represent (subjective) scores. By default, the
translation uses a linear scale (1, 2, 3, ... `C`) but custom score
functions can also be used (see the example below).

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
[`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md),
[`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
[`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
[`step_integer()`](https://recipes.tidymodels.org/dev/reference/step_integer.md),
[`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md),
[`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md),
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md),
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
fail_lvls <- c("meh", "annoying", "really_bad")

ord_data <-
  data.frame(
    item = c("paperclip", "twitter", "airbag"),
    fail_severity = factor(fail_lvls,
      levels = fail_lvls,
      ordered = TRUE
    )
  )

model.matrix(~fail_severity, data = ord_data)
#>   (Intercept) fail_severity.L fail_severity.Q
#> 1           1   -7.071068e-01       0.4082483
#> 2           1   -7.850462e-17      -0.8164966
#> 3           1    7.071068e-01       0.4082483
#> attr(,"assign")
#> [1] 0 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$fail_severity
#> [1] "contr.poly"
#> 

linear_values <- recipe(~ item + fail_severity, data = ord_data) |>
  step_dummy(item) |>
  step_ordinalscore(fail_severity)

linear_values <- prep(linear_values, training = ord_data)

bake(linear_values, new_data = NULL)
#> # A tibble: 3 × 3
#>   fail_severity item_paperclip item_twitter
#>           <int>          <dbl>        <dbl>
#> 1             1              1            0
#> 2             2              0            1
#> 3             3              0            0

custom <- function(x) {
  new_values <- c(1, 3, 7)
  new_values[as.numeric(x)]
}

nonlin_scores <- recipe(~ item + fail_severity, data = ord_data) |>
  step_dummy(item) |>
  step_ordinalscore(fail_severity, convert = custom)

tidy(nonlin_scores, number = 2)
#> # A tibble: 1 × 2
#>   terms         id                
#>   <chr>         <chr>             
#> 1 fail_severity ordinalscore_ZtdDf

nonlin_scores <- prep(nonlin_scores, training = ord_data)

bake(nonlin_scores, new_data = NULL)
#> # A tibble: 3 × 3
#>   fail_severity item_paperclip item_twitter
#>           <int>          <dbl>        <dbl>
#> 1             1              1            0
#> 2             3              0            1
#> 3             7              0            0

tidy(nonlin_scores, number = 2)
#> # A tibble: 1 × 2
#>   terms         id                
#>   <chr>         <chr>             
#> 1 fail_severity ordinalscore_ZtdDf
```
