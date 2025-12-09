# Relevel factors to a desired level

`step_relevel()` creates a *specification* of a recipe step that will
reorder the provided factor columns so that the level specified by
`ref_level` is first. This is useful for
[`contr.treatment()`](https://rdrr.io/r/stats/contrast.html) contrasts
which take the first level as the reference.

## Usage

``` r
step_relevel(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ref_level,
  objects = NULL,
  skip = FALSE,
  id = rand_id("relevel")
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

- ref_level:

  A single character value that will be used to relevel the factor
  column(s) (if the level is present).

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

The selected variables are releveled to a level (given by `ref_level`),
placing the `ref_level` in the first position.

Note that if the original columns are character, they will be converted
to factors by this step.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the value of `ref_level`

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
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data(Sacramento, package = "modeldata")
rec <- recipe(~ city + zip, data = Sacramento) |>
  step_unknown(city, new_level = "UNKNOWN") |>
  step_relevel(city, ref_level = "UNKNOWN") |>
  prep()

data <- bake(rec, Sacramento)
levels(data$city)
#>  [1] "UNKNOWN"         "ANTELOPE"        "AUBURN"         
#>  [4] "CAMERON_PARK"    "CARMICHAEL"      "CITRUS_HEIGHTS" 
#>  [7] "COOL"            "DIAMOND_SPRINGS" "EL_DORADO"      
#> [10] "EL_DORADO_HILLS" "ELK_GROVE"       "ELVERTA"        
#> [13] "FAIR_OAKS"       "FOLSOM"          "FORESTHILL"     
#> [16] "GALT"            "GARDEN_VALLEY"   "GOLD_RIVER"     
#> [19] "GRANITE_BAY"     "GREENWOOD"       "LINCOLN"        
#> [22] "LOOMIS"          "MATHER"          "MEADOW_VISTA"   
#> [25] "NORTH_HIGHLANDS" "ORANGEVALE"      "PENRYN"         
#> [28] "PLACERVILLE"     "POLLOCK_PINES"   "RANCHO_CORDOVA" 
#> [31] "RANCHO_MURIETA"  "RIO_LINDA"       "ROCKLIN"        
#> [34] "ROSEVILLE"       "SACRAMENTO"      "WALNUT_GROVE"   
#> [37] "WEST_SACRAMENTO" "WILTON"         
```
