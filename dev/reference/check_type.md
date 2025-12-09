# Quantitatively check on variables

This internal function is to be used in the prep function to ensure that
the type of the variables matches the expectation. Throws an error if
check fails.

## Usage

``` r
check_type(dat, quant = TRUE, types = NULL, call = caller_env())
```

## Arguments

- dat:

  A data frame or tibble of the training data.

- quant:

  A logical indicating whether the data is expected to be numeric (TRUE)
  or a factor/character (FALSE). Is ignored if `types` is specified.

- types:

  Character vector of allowed types. Following the same types as
  [`has_role()`](https://recipes.tidymodels.org/dev/reference/has_role.md).
  See details for more.

## Details

Using `types` is a more fine-tuned way to use this. function compared to
using `quant`. `types` should specify all allowed types as designated by
[.get_data_types](https://recipes.tidymodels.org/dev/reference/get_data_types.md).
Suppose you want to allow doubles, integers, characters, factors and
ordered factors, then you should specify
`types = c("double", "integer", "string", "factor", "ordered")` to get a
clear error message.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
