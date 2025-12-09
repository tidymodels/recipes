# Collapse infrequent categorical levels

`step_other()` creates a *specification* of a recipe step that will
potentially pool infrequently occurring values into an `"other"`
category.

## Usage

``` r
step_other(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.05,
  other = "other",
  objects = NULL,
  skip = FALSE,
  id = rand_id("other")
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

- threshold:

  A numeric value between 0 and 1, or an integer greater or equal to
  one. If less than one, then factor levels with a rate of occurrence in
  the training set below `threshold` will be pooled to `other`. If
  greater or equal to one, then this value is treated as a frequency and
  factor levels that occur less than `threshold` times will be pooled to
  `other`.

- other:

  A single character value for the other category, default to `"other"`.

- objects:

  A list of objects that contain the information to pool infrequent
  levels that is determined by
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

The overall proportion (or total counts) of the categories are computed.
The `other` category is used in place of any categorical levels whose
individual proportion (or frequency) in the training set is less than
`threshold`.

If no pooling is done the data are unmodified (although character data
may be changed to factors based on the value of `strings_as_factors` in
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)).
Otherwise, a factor is always returned with different factor levels.

If `threshold` is less than the largest category proportion, all levels
except for the most frequent are collapsed to the `other` level.

If the retained categories include the value of `other`, an error is
thrown. If `other` is in the list of discarded levels, no error occurs.

If no pooling is done, novel factor levels are converted to missing. If
pooling is needed, they will be placed into the other category.

When data to be processed contains novel levels (i.e., not contained in
the training set), the other category is assigned.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `retained` , and
`id`:

- terms:

  character, the selectors or variables selected

- retained:

  character, factor levels not pulled into `"other"`

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `threshold`: Threshold (type: double, default: 0.05)

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)

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
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data(Sacramento, package = "modeldata")

set.seed(19)
in_train <- sample(1:nrow(Sacramento), size = 800)

sacr_tr <- Sacramento[in_train, ]
sacr_te <- Sacramento[-in_train, ]

rec <- recipe(~ city + zip, data = sacr_tr)


rec <- rec |>
  step_other(city, zip, threshold = .1, other = "other values")
rec <- prep(rec, training = sacr_tr)

collapsed <- bake(rec, sacr_te)
table(sacr_te$city, collapsed$city, useNA = "always")
#>                  
#>                   ELK_GROVE SACRAMENTO other values <NA>
#>   ANTELOPE                0          0            3    0
#>   AUBURN                  0          0            0    0
#>   CAMERON_PARK            0          0            1    0
#>   CARMICHAEL              0          0            2    0
#>   CITRUS_HEIGHTS          0          0            6    0
#>   COOL                    0          0            0    0
#>   DIAMOND_SPRINGS         0          0            1    0
#>   EL_DORADO               0          0            1    0
#>   EL_DORADO_HILLS         0          0            4    0
#>   ELK_GROVE              16          0            0    0
#>   ELVERTA                 0          0            1    0
#>   FAIR_OAKS               0          0            0    0
#>   FOLSOM                  0          0            3    0
#>   FORESTHILL              0          0            0    0
#>   GALT                    0          0            2    0
#>   GARDEN_VALLEY           0          0            0    0
#>   GOLD_RIVER              0          0            1    0
#>   GRANITE_BAY             0          0            0    0
#>   GREENWOOD               0          0            0    0
#>   LINCOLN                 0          0            1    0
#>   LOOMIS                  0          0            0    0
#>   MATHER                  0          0            0    0
#>   MEADOW_VISTA            0          0            0    0
#>   NORTH_HIGHLANDS         0          0            4    0
#>   ORANGEVALE              0          0            1    0
#>   PENRYN                  0          0            0    0
#>   PLACERVILLE             0          0            1    0
#>   POLLOCK_PINES           0          0            0    0
#>   RANCHO_CORDOVA          0          0            1    0
#>   RANCHO_MURIETA          0          0            1    0
#>   RIO_LINDA               0          0            0    0
#>   ROCKLIN                 0          0            2    0
#>   ROSEVILLE               0          0            9    0
#>   SACRAMENTO              0         71            0    0
#>   WALNUT_GROVE            0          0            0    0
#>   WEST_SACRAMENTO         0          0            0    0
#>   WILTON                  0          0            0    0
#>   <NA>                    0          0            0    0

tidy(rec, number = 1)
#> # A tibble: 3 × 3
#>   terms retained   id         
#>   <chr> <chr>      <chr>      
#> 1 city  ELK_GROVE  other_HsPSC
#> 2 city  SACRAMENTO other_HsPSC
#> 3 zip   z95823     other_HsPSC

# novel levels are also "othered"
tahiti <- Sacramento[1, ]
tahiti$zip <- "a magical place"
bake(rec, tahiti)
#> Warning: ! There was 1 column that was a factor when the recipe was prepped:
#> • `zip`
#> ℹ This may cause errors when processing new data.
#> # A tibble: 1 × 2
#>   city       zip         
#>   <fct>      <fct>       
#> 1 SACRAMENTO other values

# threshold as a frequency
rec <- recipe(~ city + zip, data = sacr_tr)

rec <- rec |>
  step_other(city, zip, threshold = 2000, other = "other values")
rec <- prep(rec, training = sacr_tr)

tidy(rec, number = 1)
#> # A tibble: 2 × 3
#>   terms retained   id         
#>   <chr> <chr>      <chr>      
#> 1 city  SACRAMENTO other_2VUP1
#> 2 zip   z95823     other_2VUP1
# compare it to
# sacr_tr |> count(city, sort = TRUE) |> top_n(4)
# sacr_tr |> count(zip, sort = TRUE) |> top_n(3)
```
