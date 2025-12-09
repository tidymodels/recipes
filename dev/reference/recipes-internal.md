# Internal Functions

`ellipse_check()` is deprecated. Instead, empty selections should be
supported by all steps.

`printer()` is used for printing steps. **\[deprecated\]**

`is_trained()` is a helper function that returned a single logical to
indicate whether a recipe is traine or not.

`sel2char()` takes a list of selectors (e.g. `terms` in most steps) and
returns a character vector version for printing.

`print_step()` is used for printing steps.

## Usage

``` r
yj_transform(x, lambda, ind_neg = NULL, eps = 0.001)

estimate_yj(
  dat,
  limits = c(-5, 5),
  num_unique = 5,
  na_rm = TRUE,
  call = caller_env(2)
)

ellipse_check(...)

printer(
  tr_obj = NULL,
  untr_obj = NULL,
  trained = FALSE,
  width = max(20, options()$width - 30)
)

is_trained(x)

sel2char(x)

print_step(
  tr_obj = NULL,
  untr_obj = NULL,
  trained = FALSE,
  title = NULL,
  width = max(20, options()$width - 30),
  case_weights = NULL
)
```

## Arguments

- x:

  A list of selectors

- ...:

  Arguments pass in from a call to `step`.

- tr_obj:

  A character vector of names that have been resolved during preparing
  the recipe (e.g. the `columns` object of
  [`step_log()`](https://recipes.tidymodels.org/dev/reference/step_log.md)).

- untr_obj:

  An object of selectors prior to prepping the recipe (e.g. `terms` in
  most steps).

- trained:

  A logical for whether the step has been trained.

- width:

  An integer denoting where the output should be wrapped.

- title:

  A character, shortly describing the action the step takes.

## Value

`ellipse_check()`: If not empty, a list of quosures. If empty, an error
is thrown.

`printer()`: `NULL`, invisibly.

`is_trained()`: A single logical.

`sel2char()`: A character vector.

`print_step()`: `NULL`, invisibly.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
