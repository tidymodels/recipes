# Assign missing categories to "unknown"

`step_unknown()` creates a *specification* of a recipe step that will
assign a missing value in a factor level to `"unknown"`.

## Usage

``` r
step_unknown(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  new_level = "unknown",
  objects = NULL,
  skip = FALSE,
  id = rand_id("unknown")
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

- new_level:

  A single character value that will be assigned to new factor levels.

- objects:

  A list of objects that contain the information on factor levels that
  will be determined by
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

The selected variables are adjusted to have a new level (given by
`new_level`) that is placed in the last position.

Note that if the original columns are character, they will be converted
to factors by this step.

If `new_level` is already in the data given to
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), an
error is thrown.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- statistic:

  character, the factor levels for the new values

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

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
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md),
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data(Sacramento, package = "modeldata")

rec <-
  recipe(~ city + zip, data = Sacramento) |>
  step_unknown(city, new_level = "unknown city") |>
  step_unknown(zip, new_level = "unknown zip") |>
  prep()

table(bake(rec, new_data = NULL) |> pull(city),
  Sacramento |> pull(city),
  useNA = "always"
) |>
  as.data.frame() |>
  dplyr::filter(Freq > 0)
#>               Var1            Var2 Freq
#> 1         ANTELOPE        ANTELOPE   33
#> 2           AUBURN          AUBURN    5
#> 3     CAMERON_PARK    CAMERON_PARK    9
#> 4       CARMICHAEL      CARMICHAEL   20
#> 5   CITRUS_HEIGHTS  CITRUS_HEIGHTS   35
#> 6             COOL            COOL    1
#> 7  DIAMOND_SPRINGS DIAMOND_SPRINGS    1
#> 8        EL_DORADO       EL_DORADO    2
#> 9  EL_DORADO_HILLS EL_DORADO_HILLS   23
#> 10       ELK_GROVE       ELK_GROVE  114
#> 11         ELVERTA         ELVERTA    4
#> 12       FAIR_OAKS       FAIR_OAKS    9
#> 13          FOLSOM          FOLSOM   17
#> 14      FORESTHILL      FORESTHILL    1
#> 15            GALT            GALT   21
#> 16   GARDEN_VALLEY   GARDEN_VALLEY    1
#> 17      GOLD_RIVER      GOLD_RIVER    4
#> 18     GRANITE_BAY     GRANITE_BAY    3
#> 19       GREENWOOD       GREENWOOD    1
#> 20         LINCOLN         LINCOLN   22
#> 21          LOOMIS          LOOMIS    2
#> 22          MATHER          MATHER    1
#> 23    MEADOW_VISTA    MEADOW_VISTA    1
#> 24 NORTH_HIGHLANDS NORTH_HIGHLANDS   21
#> 25      ORANGEVALE      ORANGEVALE   11
#> 26          PENRYN          PENRYN    1
#> 27     PLACERVILLE     PLACERVILLE   10
#> 28   POLLOCK_PINES   POLLOCK_PINES    3
#> 29  RANCHO_CORDOVA  RANCHO_CORDOVA   28
#> 30  RANCHO_MURIETA  RANCHO_MURIETA    3
#> 31       RIO_LINDA       RIO_LINDA   13
#> 32         ROCKLIN         ROCKLIN   17
#> 33       ROSEVILLE       ROSEVILLE   48
#> 34      SACRAMENTO      SACRAMENTO  438
#> 35    WALNUT_GROVE    WALNUT_GROVE    1
#> 36 WEST_SACRAMENTO WEST_SACRAMENTO    3
#> 37          WILTON          WILTON    5

tidy(rec, number = 1)
#> # A tibble: 1 × 3
#>   terms value        id           
#>   <chr> <chr>        <chr>        
#> 1 city  unknown city unknown_evI1V
```
