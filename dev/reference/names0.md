# Naming Tools

`names0()` creates a series of `num` names with a common prefix. The
names are numbered with leading zeros (e.g. `prefix01`-`prefix10`
instead of `prefix1`-`prefix10`). `dummy_names` can be used for renaming
unordered and ordered dummy variables (in
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)).

## Usage

``` r
names0(num, prefix = "x", call = rlang::caller_env())

dummy_names(var, lvl, ordinal = FALSE, sep = "_")

dummy_extract_names(var, lvl, ordinal = FALSE, sep = "_")
```

## Arguments

- num:

  A single integer for how many elements are created.

- prefix:

  A character string that will start each name.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

- var:

  A single string for the original factor name.

- lvl:

  A character vectors of the factor levels (in order). When used with
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
  `lvl` would be the suffixes that result *after* `model.matrix` is
  called (see the example below).

- ordinal:

  A logical; was the original factor ordered?

- sep:

  A single character value for the separator between the names and
  levels.

## Value

`names0()` returns a character string of length `num` and
`dummy_names()` generates a character vector the same length as `lvl`.

## Details

When using `dummy_names()`, factor levels that are not valid variable
names (e.g. "some text with spaces") will be changed to valid names by
[`base::make.names()`](https://rdrr.io/r/base/make.names.html); see
example below. This function will also change the names of ordinal dummy
variables. Instead of values such as `".L"`, `".Q"`, or `"^4"`, ordinal
dummy variables are given simple integer suffixes such as `"_1"`,
`"_2"`, etc.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
names0(9, "a")
#> [1] "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9"
names0(10, "a")
#>  [1] "a01" "a02" "a03" "a04" "a05" "a06" "a07" "a08" "a09" "a10"

example <- data.frame(
  x = ordered(letters[1:5]),
  y = factor(LETTERS[1:5]),
  z = factor(paste(LETTERS[1:5], 1:5))
)

dummy_names("y", levels(example$y)[-1])
#> [1] "y_B" "y_C" "y_D" "y_E"
dummy_names("z", levels(example$z)[-1])
#> [1] "z_B.2" "z_C.3" "z_D.4" "z_E.5"

after_mm <- colnames(model.matrix(~x, data = example))[-1]
after_mm
#> [1] "x.L" "x.Q" "x.C" "x^4"
levels(example$x)
#> [1] "a" "b" "c" "d" "e"

dummy_names("x", substring(after_mm, 2), ordinal = TRUE)
#> [1] "x_1" "x_2" "x_3" "x_4"
```
