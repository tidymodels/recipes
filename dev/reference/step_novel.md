# Simple value assignments for novel factor levels

`step_novel()` creates a *specification* of a recipe step that will
assign a previously unseen factor level to `"new"`.

## Usage

``` r
step_novel(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  new_level = "new",
  objects = NULL,
  skip = FALSE,
  id = rand_id("novel")
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
`new_level`) that is placed in the last position. During preparation
there will be no data points associated with this new level since all of
the data have been seen.

Note that if the original columns are character, they will be converted
to factors by this step.

Missing values will remain missing.

If `new_level` is already in the data given to
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), an
error is thrown.

When fitting a model that can deal with new factor levels, consider
using
[`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)
with `allow_novel_levels = TRUE` set in
[`hardhat::default_recipe_blueprint()`](https://hardhat.tidymodels.org/reference/default_recipe_blueprint.html).
This will allow your model to handle new levels at prediction time,
instead of throwing warnings or errors.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the factor levels that are used for the new value

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

sacr_tr <- Sacramento[1:800, ]
sacr_te <- Sacramento[801:806, ]

# Without converting the predictor to a character, the new level would be converted
# to `NA`.
sacr_te$city <- as.character(sacr_te$city)
sacr_te$city[3] <- "beeptown"
sacr_te$city[4] <- "boopville"
sacr_te$city <- as.factor(sacr_te$city)

rec <- recipe(~ city + zip, data = sacr_tr)

rec <- rec |>
  step_novel(city, zip)
rec <- prep(rec, training = sacr_tr)

processed <- bake(rec, sacr_te)
tibble(old = sacr_te$city, new = processed$city)
#> # A tibble: 6 × 2
#>   old        new       
#>   <fct>      <fct>     
#> 1 SACRAMENTO SACRAMENTO
#> 2 AUBURN     AUBURN    
#> 3 beeptown   new       
#> 4 boopville  new       
#> 5 SACRAMENTO SACRAMENTO
#> 6 ROSEVILLE  ROSEVILLE 

tidy(rec, number = 1)
#> # A tibble: 2 × 3
#>   terms value id         
#>   <chr> <chr> <chr>      
#> 1 city  new   novel_1VT1q
#> 2 zip   new   novel_1VT1q
```
