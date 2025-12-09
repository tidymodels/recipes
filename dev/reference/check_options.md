# Check that options argument contain the right elements

`check_options()` is to be used in the prep function to ensure that
`options` arguments are lists and contain the right elements.

## Usage

``` r
check_options(options, exclude = NULL, include = NULL, call = caller_env())
```

## Arguments

- options:

  options to be checked.

- exclude:

  Character vector, elements that can't be present in `options`.

- include:

  Character vector, Allowed elements in `options`.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
