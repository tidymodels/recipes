# Update packages

This will check to see if all required packages are installed.

## Usage

``` r
recipes_pkg_check(pkg = NULL, ...)
```

## Arguments

- pkg:

  A character string for the package being checked

- ...:

  Extra arguments to pass to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)

## Value

Nothing is returned but a message is printed to the console about which
packages (if any) should be installed along with code to do so.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipes_pkg_check("recipes")
} # }
```
