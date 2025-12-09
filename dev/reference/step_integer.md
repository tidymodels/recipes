# Convert values to predefined integers

`step_integer()` creates a specification of a recipe step that will
convert data into a set of ascending integers based on the ascending
order from the training data. Also known as integer encoding.

## Usage

``` r
step_integer(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  strict = TRUE,
  zero_based = FALSE,
  key = NULL,
  skip = FALSE,
  id = rand_id("integer")
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

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- strict:

  A logical for whether the values should be returned as integers (as
  opposed to double).

- zero_based:

  A logical for whether the integers should start at zero and new values
  be appended as the largest integer.

- key:

  A list that contains the information needed to create integer
  variables for each variable contained in `terms`. This is `NULL` until
  the step is trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

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

`step_integer()` will determine the unique values of each variable from
the training set (excluding missing values), order them, and then assign
integers to each value. When baked, each data point is translated to its
corresponding integer or a value of zero for yet unseen data (although
see the `zero_based` argument above). Missing values propagate.

Factor inputs are ordered by their levels. All others are ordered by
[`sort()`](https://rdrr.io/r/base/sort.html).

Despite the name, the new values are returned as numeric unless
`strict = TRUE`, which will coerce the results to integers.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  list, a *list column* with the conversion key

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

sacr_tr <- Sacramento[1:100, ]
sacr_tr$sqft[1] <- NA

sacr_te <- Sacramento[101:105, ]
sacr_te$sqft[1] <- NA
sacr_te$city[1] <- "whoville"
#> Warning: invalid factor level, NA generated
sacr_te$city[2] <- NA

rec <- recipe(type ~ ., data = sacr_tr) |>
  step_integer(all_predictors()) |>
  prep(training = sacr_tr)

bake(rec, sacr_te, all_predictors())
#> # A tibble: 5 × 8
#>    city   zip  beds baths  sqft price latitude longitude
#>   <int> <int> <int> <int> <int> <int>    <int>     <int>
#> 1    NA    35     4     2    NA     0        0         0
#> 2    NA    62     3     2     0     0        0         0
#> 3    28    34     3     2    56     0        0         0
#> 4    34    51     3     1     0     0        0         0
#> 5    34    58     4     3     0     0        0         0
tidy(rec, number = 1)
#> # A tibble: 8 × 3
#>   terms     value             id           
#>   <chr>     <list>            <chr>        
#> 1 city      <tibble [37 × 2]> integer_ckWIU
#> 2 zip       <tibble [68 × 2]> integer_ckWIU
#> 3 beds      <tibble [5 × 2]>  integer_ckWIU
#> 4 baths     <tibble [4 × 2]>  integer_ckWIU
#> 5 sqft      <tibble [94 × 2]> integer_ckWIU
#> 6 price     <tibble [95 × 2]> integer_ckWIU
#> 7 latitude  <tibble [99 × 2]> integer_ckWIU
#> 8 longitude <tibble [99 × 2]> integer_ckWIU
```
