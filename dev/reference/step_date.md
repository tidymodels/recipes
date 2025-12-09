# Date feature generator

`step_date()` creates a *specification* of a recipe step that will
convert date data into one or more factor or numeric variables.

## Usage

``` r
step_date(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  features = c("dow", "month", "year"),
  abbr = TRUE,
  label = TRUE,
  ordinal = FALSE,
  locale = clock::clock_locale()$labels,
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("date")
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

- features:

  A character string that includes at least one of the following values:
  `month`, `dow` (day of week), `mday` (day of month), `doy` (day of
  year), `week`, `month`, `decimal` (decimal date, e.g. 2002.197),
  `quarter`, `semester`, `year`.

- abbr:

  A logical. Only available for features `month` or `dow`. `FALSE` will
  display the day of the week as an ordered factor of character strings,
  such as "Sunday". `TRUE` will display an abbreviated version of the
  label, such as "Sun". `abbr` is disregarded if `label = FALSE`.

- label:

  A logical. Only available for features `month` or `dow`. `TRUE` will
  display the day of the week as an ordered factor of character strings,
  such as "Sunday." `FALSE` will display the day of the week as a
  number.

- ordinal:

  A logical: should factors be ordered? Only available for features
  `month` or `dow`.

- locale:

  Locale to be used for `month` and `dow`, see
  [locales](https://rdrr.io/r/base/locales.html). On Linux systems you
  can use `system("locale -a")` to list all the installed locales. Can
  be a locales string, or a
  [`clock::clock_labels()`](https://clock.r-lib.org/reference/clock_labels.html)
  object. Defaults to `clock::clock_locale()$labels`.

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

Unlike some other steps, `step_date()` does *not* remove the original
date variables by default. Set `keep_original_cols` to `FALSE` to remove
them.

See
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md)
if you want to calculate features that are smaller than days.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value`, `ordinal`
, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the feature names

- ordinal:

  logical, are factors ordered

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
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
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
library(lubridate)
#> 
#> Attaching package: ‘lubridate’
#> The following objects are masked from ‘package:base’:
#> 
#>     date, intersect, setdiff, union

examples <- data.frame(
  Dan = ymd("2002-03-04") + days(1:10),
  Stefan = ymd("2006-01-13") + days(1:10)
)
date_rec <- recipe(~ Dan + Stefan, examples) |>
  step_date(all_predictors())

tidy(date_rec, number = 1)
#> # A tibble: 3 × 4
#>   terms            value ordinal id        
#>   <chr>            <chr> <lgl>   <chr>     
#> 1 all_predictors() dow   FALSE   date_2jjSe
#> 2 all_predictors() month FALSE   date_2jjSe
#> 3 all_predictors() year  FALSE   date_2jjSe

date_rec <- prep(date_rec, training = examples)

date_values <- bake(date_rec, new_data = examples)
date_values
#> # A tibble: 10 × 8
#>    Dan        Stefan     Dan_dow Dan_month Dan_year Stefan_dow
#>    <date>     <date>     <fct>   <fct>        <int> <fct>     
#>  1 2002-03-05 2006-01-14 Tue     Mar           2002 Sat       
#>  2 2002-03-06 2006-01-15 Wed     Mar           2002 Sun       
#>  3 2002-03-07 2006-01-16 Thu     Mar           2002 Mon       
#>  4 2002-03-08 2006-01-17 Fri     Mar           2002 Tue       
#>  5 2002-03-09 2006-01-18 Sat     Mar           2002 Wed       
#>  6 2002-03-10 2006-01-19 Sun     Mar           2002 Thu       
#>  7 2002-03-11 2006-01-20 Mon     Mar           2002 Fri       
#>  8 2002-03-12 2006-01-21 Tue     Mar           2002 Sat       
#>  9 2002-03-13 2006-01-22 Wed     Mar           2002 Sun       
#> 10 2002-03-14 2006-01-23 Thu     Mar           2002 Mon       
#> # ℹ 2 more variables: Stefan_month <fct>, Stefan_year <int>

tidy(date_rec, number = 1)
#> # A tibble: 6 × 4
#>   terms  value ordinal id        
#>   <chr>  <chr> <lgl>   <chr>     
#> 1 Dan    dow   FALSE   date_2jjSe
#> 2 Dan    month FALSE   date_2jjSe
#> 3 Dan    year  FALSE   date_2jjSe
#> 4 Stefan dow   FALSE   date_2jjSe
#> 5 Stefan month FALSE   date_2jjSe
#> 6 Stefan year  FALSE   date_2jjSe
```
