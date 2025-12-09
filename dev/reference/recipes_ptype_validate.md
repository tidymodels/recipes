# Validate prototype of recipe object

This helper function validates a dataframe against the ptype of a
recipe.

## Usage

``` r
recipes_ptype_validate(
  x,
  new_data,
  ...,
  stage = "prep",
  call = rlang::caller_env()
)
```

## Arguments

- x:

  A `recipe` object.

- new_data:

  A data.frame. To be patched aganist ptype of `x`.

- ...:

  currently not used.

- stage:

  A single character. Must be one of `"prep"` or `"bake"`. See details
  for more. Defaults to `"prep"`.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.html). The
  function will be mentioned in error messages as the source of the
  error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## Value

Nothing or an error.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
[recipes_ptype](https://recipes.tidymodels.org/dev/reference/recipes_ptype.md)

## Examples

``` r
rec <- recipe(mpg ~ disp, data = mtcars)

recipes_ptype_validate(rec, mtcars)
```
