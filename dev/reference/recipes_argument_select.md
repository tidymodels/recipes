# Evaluate a selection with tidyselect semantics for arguments

`recipes_argument_select()` is a variant of
[`recipes_eval_select()`](https://recipes.tidymodels.org/dev/reference/recipes_eval_select.md)
that is tailored to work well with arguments in steps that specify
variables. Such as `denom` in
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md).

This is a developer tool that is only useful for creating new recipes
steps.

## Usage

``` r
recipes_argument_select(
  quos,
  data,
  info,
  single = TRUE,
  arg_name = "outcome",
  call = caller_env()
)
```

## Arguments

- quos:

  A list of quosures describing the selection. Captured with
  [`rlang::enquos()`](https://rlang.r-lib.org/reference/enquo.html) and
  stored in the step object corresponding to the argument.

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

- single:

  A logical. Should an error be thrown if more than 1 variable is
  selected. Defaults to `TRUE`.

- arg_name:

  A string. Name of argument, used to enrich error messages.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.html). The
  function will be mentioned in error messages as the source of the
  error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## Value

A character vector containing the evaluated selection.

## Details

This function is written to be backwards compatible with previous input
types of these arguments. Will thus accept strings, tidyselect, recipes
selections, helper functions
[`imp_vars()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md)
in addition to the prefered bare names.

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

recipes_argument_select(quos(Year), scat, info)
#> [1] "Year"
recipes_argument_select(vars(Year), scat, info)
#> [1] "Year"
recipes_argument_select(imp_vars(Year), scat, info)
#> [1] "Year"
```
