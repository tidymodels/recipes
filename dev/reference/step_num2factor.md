# Convert numbers to factors

`step_num2factor()` will convert one or more numeric vectors to factors
(ordered or unordered). This can be useful when categories are encoded
as integers.

## Usage

``` r
step_num2factor(
  recipe,
  ...,
  role = NA,
  transform = function(x) x,
  trained = FALSE,
  levels,
  ordered = FALSE,
  skip = FALSE,
  id = rand_id("num2factor")
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

- transform:

  A function taking a single argument `x` that can be used to modify the
  numeric values prior to determining the levels (perhaps using
  [`base::as.integer()`](https://rdrr.io/r/base/integer.html) or
  [`base::as.factor()`](https://rdrr.io/r/base/factor.html)). The output
  of a function should be an integer that corresponds to the value of
  `levels` that should be assigned. If not an integer, the value will be
  converted to an integer during
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- levels:

  A character vector of values that will be used as the levels. These
  are the numeric data converted to character and ordered. This is
  modified once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  executed.

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

Note that since the numeric variables will be used for indexing into
`levels` it will need to take values between `1` and `length(levels)` to
avoid getting `NA`s as results. Using `transform = base::as.factor` can
be used to shrink values to smaller domain.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `ordered` , and
`id`:

- terms:

  character, the selectors or variables selected

- ordered:

  logical, were the factor(s) ordered

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
library(dplyr)
data(attrition, package = "modeldata")

attrition |>
  group_by(StockOptionLevel) |>
  count()
#> # A tibble: 4 × 2
#> # Groups:   StockOptionLevel [4]
#>   StockOptionLevel     n
#>              <int> <int>
#> 1                0   631
#> 2                1   596
#> 3                2   158
#> 4                3    85

amnt <- c("nothin", "meh", "some", "copious")

rec <-
  recipe(Attrition ~ StockOptionLevel, data = attrition) |>
  step_num2factor(
    StockOptionLevel,
    transform = function(x) x + 1,
    levels = amnt
  )

encoded <- rec |>
  prep() |>
  bake(new_data = NULL)

table(encoded$StockOptionLevel, attrition$StockOptionLevel)
#>          
#>             0   1   2   3
#>   nothin  631   0   0   0
#>   meh       0 596   0   0
#>   some      0   0 158   0
#>   copious   0   0   0  85


# an example for binning

binner <- function(x) {
  x <- cut(x, breaks = 1000 * c(0, 5, 10, 20), include.lowest = TRUE)
  # now return the group number
  as.numeric(x)
}

inc <- c("low", "med", "high")

rec <-
  recipe(Attrition ~ MonthlyIncome, data = attrition) |>
  step_num2factor(
    MonthlyIncome,
    transform = binner,
    levels = inc,
    ordered = TRUE
  ) |>
  prep()

encoded <- bake(rec, new_data = NULL)

table(encoded$MonthlyIncome, binner(attrition$MonthlyIncome))
#>       
#>          1   2   3
#>   low  749   0   0
#>   med    0 440   0
#>   high   0   0 281

# What happens when a value is out of range?
ceo <- attrition |>
  slice(1) |>
  mutate(MonthlyIncome = 10^10)

bake(rec, ceo)
#> # A tibble: 1 × 2
#>   MonthlyIncome Attrition
#>   <ord>         <fct>    
#> 1 NA            Yes      
```
