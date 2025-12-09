# Holiday feature generator

`step_holiday()` creates a *specification* of a recipe step that will
convert date data into one or more binary indicator variables for common
holidays.

## Usage

``` r
step_holiday(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  holidays = c("LaborDay", "NewYearsDay", "ChristmasDay"),
  columns = NULL,
  sparse = "auto",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("holiday")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. The
  selected variables should have class `Date` or `POSIXct`. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- holidays:

  A character string that includes at least one holiday supported by the
  `timeDate` package. See
  [`timeDate::listHolidays()`](https://geobosh.github.io/timeDateDoc/reference/holiday-Listing.html)
  for a complete list.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- sparse:

  A single string. Should the columns produced be sparse vectors. Can
  take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
  then workflows can determine the best option. Defaults to `"auto"`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `TRUE`.

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

Unlike some other steps, `step_holiday()` does *not* remove the original
date variables by default. Set `keep_original_cols` to `FALSE` to remove
them.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `holiday` , and
`id`:

- terms:

  character, the selectors or variables selected

- holiday:

  character, name of holidays

- id:

  character, id of this step

## Sparse data

This step produces sparse columns if `sparse = "yes"` is being set. The
default value `"auto"` won't trigger production fo sparse columns if a
recipe is
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)ed, but
allows for a workflow to toggle to `"yes"` or `"no"` depending on
whether the model supports
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
and if the model is is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't
perfect, and there will be times when you want to manually overwrite by
setting `sparse = "yes"` or `sparse = "no"`.

## Case weights

The underlying operation does not allow for case weights.

## See also

[`timeDate::listHolidays()`](https://geobosh.github.io/timeDateDoc/reference/holiday-Listing.html)

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
[`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md),
[`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md),
[`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md),
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
library(lubridate)

examples <- data.frame(someday = ymd("2000-12-20") + days(0:40))
holiday_rec <- recipe(~someday, examples) |>
  step_holiday(all_predictors())

holiday_rec <- prep(holiday_rec, training = examples)
holiday_values <- bake(holiday_rec, new_data = examples)
holiday_values
#> # A tibble: 41 × 4
#>    someday    someday_LaborDay someday_NewYearsDay someday_ChristmasDay
#>    <date>                <int>               <int>                <int>
#>  1 2000-12-20                0                   0                    0
#>  2 2000-12-21                0                   0                    0
#>  3 2000-12-22                0                   0                    0
#>  4 2000-12-23                0                   0                    0
#>  5 2000-12-24                0                   0                    0
#>  6 2000-12-25                0                   0                    1
#>  7 2000-12-26                0                   0                    0
#>  8 2000-12-27                0                   0                    0
#>  9 2000-12-28                0                   0                    0
#> 10 2000-12-29                0                   0                    0
#> # ℹ 31 more rows
```
