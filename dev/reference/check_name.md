# check that newly created variable names don't overlap

`check_name` is to be used in the bake function to ensure that newly
created variable names don't overlap with existing names. Throws an
error if check fails.

## Usage

``` r
check_name(
  res,
  new_data,
  object,
  newname = NULL,
  names = FALSE,
  call = caller_env()
)
```

## Arguments

- res:

  A data frame or tibble of the newly created variables.

- new_data:

  A data frame or tibble passed to the bake function.

- object:

  A trained object passed to the bake function.

- newname:

  A string of variable names if prefix isn't specified in the trained
  object.

- names:

  A logical determining if the names should be set using the names
  function (TRUE) or colnames function (FALSE).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
