# Evaluate a selection with tidyselect semantics specific to recipes

`recipes_eval_select()` is a recipes specific variant of
[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html)
enhanced with the ability to recognize recipes selectors, such as
[`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md).
See
[selections](https://recipes.tidymodels.org/dev/reference/selections.md)
for more information about the unique recipes selectors.

This is a developer tool that is only useful for creating new recipes
steps.

## Usage

``` r
recipes_eval_select(
  quos,
  data,
  info,
  ...,
  allow_rename = FALSE,
  check_case_weights = TRUE,
  strict = TRUE,
  call = caller_env()
)
```

## Arguments

- quos:

  A list of quosures describing the selection. This is generally the
  `...` argument of your step function, captured with
  [`rlang::enquos()`](https://rlang.r-lib.org/reference/enquo.html) and
  stored in the step object as the `terms` element.

- data:

  A data frame to use as the context to evaluate the selection in. This
  is generally the `training` data passed to the
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  method of your step.

- info:

  A data frame of term information describing each column's type and
  role for use with the recipes selectors. This is generally the `info`
  data passed to the
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  method of your step.

- ...:

  These dots are for future extensions and must be empty.

- allow_rename:

  Should the renaming syntax `c(foo = bar)` be allowed? This is rarely
  required, and is currently only used by
  [`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md).
  It is unlikely that your step will need renaming capabilities.

- check_case_weights:

  Should selecting case weights throw an error? Defaults to `TRUE`. This
  is rarely changed and only needed in
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md),
  [`bake.recipe()`](https://recipes.tidymodels.org/dev/reference/bake.md),
  [`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
  and
  [`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).

- strict:

  Should selecting non-existing names throw an error? Defaults to
  `TRUE`. This is rarely changed and only needed in
  \`.recipes_estimate_sparsity.recipe()“.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.html). The
  function will be mentioned in error messages as the source of the
  error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## Value

A named character vector containing the evaluated selection. The names
are always the same as the values, except when `allow_rename = TRUE`, in
which case the names reflect the new names chosen by the user.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
library(rlang)
data(scat, package = "modeldata")

rec <- recipe(Species ~ ., data = scat)

info <- summary(rec)
info
#> # A tibble: 19 × 4
#>    variable  type      role      source  
#>    <chr>     <list>    <chr>     <chr>   
#>  1 Month     <chr [3]> predictor original
#>  2 Year      <chr [2]> predictor original
#>  3 Site      <chr [3]> predictor original
#>  4 Location  <chr [3]> predictor original
#>  5 Age       <chr [2]> predictor original
#>  6 Number    <chr [2]> predictor original
#>  7 Length    <chr [2]> predictor original
#>  8 Diameter  <chr [2]> predictor original
#>  9 Taper     <chr [2]> predictor original
#> 10 TI        <chr [2]> predictor original
#> 11 Mass      <chr [2]> predictor original
#> 12 d13C      <chr [2]> predictor original
#> 13 d15N      <chr [2]> predictor original
#> 14 CN        <chr [2]> predictor original
#> 15 ropey     <chr [2]> predictor original
#> 16 segmented <chr [2]> predictor original
#> 17 flat      <chr [2]> predictor original
#> 18 scrape    <chr [2]> predictor original
#> 19 Species   <chr [3]> outcome   original

quos <- quos(all_numeric_predictors(), where(is.factor))

recipes_eval_select(quos, scat, info)
#>        Year         Age      Number      Length    Diameter 
#>      "Year"       "Age"    "Number"    "Length"  "Diameter" 
#>       Taper          TI        Mass        d13C        d15N 
#>     "Taper"        "TI"      "Mass"      "d13C"      "d15N" 
#>          CN       ropey   segmented        flat      scrape 
#>        "CN"     "ropey" "segmented"      "flat"    "scrape" 
#>     Species       Month        Site    Location 
#>   "Species"     "Month"      "Site"  "Location" 
```
