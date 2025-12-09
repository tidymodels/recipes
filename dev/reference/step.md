# `step()` sets the class of the `step` and `check()` is for checks.

`step()` sets the class of the `step` and `check()` is for checks.

## Usage

``` r
step(subclass, ..., .prefix = "step_", call = rlang::caller_env(2))

check(subclass, ..., .prefix = "check_", call = rlang::caller_env(2))
```

## Arguments

- subclass:

  A character string for the resulting class. For example, if
  `subclass = "blah"` the step object that is returned has class
  `step_blah` or `check_blah` depending on the context.

- ...:

  All arguments to the operator that should be returned. Required
  arguments are `trained`, `skip`, and `id`.

- .prefix:

  Prefix to the subclass created.

## Value

An updated step or check with the new class.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
