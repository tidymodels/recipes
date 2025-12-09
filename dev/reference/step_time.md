# Time feature generator

`step_time()` creates a *specification* of a recipe step that will
convert date-time data into one or more factor or numeric variables.

## Usage

``` r
step_time(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  features = c("hour", "minute", "second"),
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("time")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. The
  selected variables should have class `POSIXct` or `POSIXlt`. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- features:

  A character string that includes at least one of the following values:
  `am` (is is AM), `hour`, `hour12`, `minute`, `second`, `decimal_day`.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

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

Unlike some other steps, `step_time()` does *not* remove the original
time variables by default. Set `keep_original_cols` to `FALSE` to remove
them.

`decimal_day` return time of day as a decimal number between 0 and 24.
for example `"07:15:00"` would be transformed to `7.25` and `"03:59:59"`
would be transformed to `3.999722`. The formula for these calculations
are \`hour(x)

- (second(x) + minute(x) \* 60) / 3600\`.

See
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md)
if you want to calculate features that are larger than hours.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the feature names

- id:

  character, id of this step

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
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
library(lubridate)

examples <- data.frame(
  times = ymd_hms("2022-05-06 23:51:07") +
  hours(1:5) + minutes(1:5) + seconds(1:5)
)
time_rec <- recipe(~ times, examples) |>
  step_time(all_predictors())

tidy(time_rec, number = 1)
#> # A tibble: 3 × 3
#>   terms            value  id        
#>   <chr>            <chr>  <chr>     
#> 1 all_predictors() hour   time_0ZB2l
#> 2 all_predictors() minute time_0ZB2l
#> 3 all_predictors() second time_0ZB2l

time_rec <- prep(time_rec, training = examples)

time_values <- bake(time_rec, new_data = examples)
time_values
#> # A tibble: 5 × 4
#>   times               times_hour times_minute times_second
#>   <dttm>                   <int>        <int>        <dbl>
#> 1 2022-05-07 00:52:08          0           52            8
#> 2 2022-05-07 01:53:09          1           53            9
#> 3 2022-05-07 02:54:10          2           54           10
#> 4 2022-05-07 03:55:11          3           55           11
#> 5 2022-05-07 04:56:12          4           56           12

tidy(time_rec, number = 1)
#> # A tibble: 3 × 3
#>   terms value  id        
#>   <chr> <chr>  <chr>     
#> 1 times hour   time_0ZB2l
#> 2 times minute time_0ZB2l
#> 3 times second time_0ZB2l
```
