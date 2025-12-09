# Helpers for steps with case weights

These functions can be used to do basic calculations with or without
case weights.

## Usage

``` r
get_case_weights(info, .data, call = rlang::caller_env())

averages(x, wts = NULL, na_rm = TRUE)

medians(x, wts = NULL)

variances(x, wts = NULL, na_rm = TRUE)

correlations(x, wts = NULL, use = "everything", method = "pearson")

covariances(x, wts = NULL, use = "everything", method = "pearson")

pca_wts(x, wts = NULL)

are_weights_used(wts, unsupervised = FALSE)
```

## Arguments

- info:

  A data frame from the `info` argument within steps

- .data:

  The training data

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x:

  A numeric vector or a data frame

- wts:

  A vector of case weights

- na_rm:

  A logical value indicating whether `NA` values should be removed
  during computations.

- use:

  Used by `correlations()` or `covariances()` to pass argument to
  [`cor()`](https://rdrr.io/r/stats/cor.html) or
  [`cov()`](https://rdrr.io/r/stats/cor.html)

- method:

  Used by `correlations()` or `covariances()` to pass argument to
  [`cor()`](https://rdrr.io/r/stats/cor.html) or
  [`cov()`](https://rdrr.io/r/stats/cor.html)

- unsupervised:

  Can the step handle unsupervised weights

## Details

`get_case_weights()` is designed for developers of recipe steps, to
return a column with the role of "case weight" as a vector.

For the other functions, rows with missing case weights are removed from
calculations.

For `averages()` and `variances()`, missing values in the data (*not*
the case weights) only affect the calculations for those rows. For
`correlations()`, the correlation matrix computation first removes rows
with any missing values (equal to the "complete.obs" strategy in
[`stats::cor()`](https://rdrr.io/r/stats/cor.html)).

`are_weights_used()` is designed for developers of recipe steps and is
used inside print method to determine how printing should be done.

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)
