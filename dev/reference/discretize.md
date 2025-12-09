# Discretize Numeric Variables

`discretize()` converts a numeric vector into a factor with bins having
approximately the same number of data points (based on a training set).

## Usage

``` r
discretize(x, ...)

# Default S3 method
discretize(x, ...)

# S3 method for class 'numeric'
discretize(
  x,
  cuts = 4,
  labels = NULL,
  prefix = "bin",
  keep_na = TRUE,
  infs = TRUE,
  min_unique = 10,
  ...
)

# S3 method for class 'discretize'
predict(object, new_data, ...)
```

## Arguments

- x:

  A numeric vector

- ...:

  Options to pass to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) that
  should not include `x` or `probs`.

- cuts:

  An integer defining how many cuts to make of the data.

- labels:

  A character vector defining the factor levels that will be in the new
  factor (from smallest to largest). This should have length `cuts+1`
  and should not include a level for missing (see `keep_na` below).

- prefix:

  A single parameter value to be used as a prefix for the factor levels
  (e.g. `bin1`, `bin2`, ...). If the string is not a valid R name, it is
  coerced to one. If `prefix = NULL` then the factor levels will be
  labelled according to the output of
  [`cut()`](https://rdrr.io/r/base/cut.html).

- keep_na:

  A logical for whether a factor level should be created to identify
  missing values in `x`. If `keep_na` is set to `TRUE` then
  `na.rm = TRUE` is used when calling
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- infs:

  A logical indicating whether the smallest and largest cut point should
  be infinite.

- min_unique:

  An integer defining a sample size line of dignity for the binning. If
  `(the number of unique values)/(cuts+1)` is less than `min_unique`, no
  discretization takes place.

- object:

  An object of class `discretize`.

- new_data:

  A new numeric object to be binned.

## Value

`discretize` returns an object of class `discretize` and
`predict.discretize()` returns a factor vector.

## Details

`discretize()` estimates the cut points from `x` using percentiles. For
example, if `cuts = 3`, the function estimates the quartiles of `x` and
uses these as the cut points. If `cuts = 2`, the bins are defined as
being above or below the median of `x`.

The [`predict()`](https://rdrr.io/r/stats/predict.html) method can then
be used to turn numeric vectors into factor vectors.

If `keep_na = TRUE`, a suffix of `"_missing"` is used as a factor level
(see the examples below).

If `infs = FALSE` and a new value is greater than the largest value of
`x`, a missing value will result.

## Examples

``` r
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

median(biomass_tr$carbon)
#> [1] 47.1
discretize(biomass_tr$carbon, cuts = 2)
#> Bins: 3 (includes missing category)
#> Breaks: -Inf, 47.1, Inf
discretize(biomass_tr$carbon, cuts = 2, infs = FALSE)
#> Bins: 3 (includes missing category)
#> Breaks: 14.61, 47.1, 97.18
discretize(biomass_tr$carbon, cuts = 2, infs = FALSE, keep_na = FALSE)
#> Bins: 2
#> Breaks: 14.61, 47.1, 97.18
discretize(biomass_tr$carbon, cuts = 2, prefix = "maybe a bad idea to bin")
#> Warning: The prefix "maybe a bad idea to bin" is not a valid R name. It has
#> been changed to "maybe.a.bad.idea.to.bin".
#> Bins: 3 (includes missing category)
#> Breaks: -Inf, 47.1, Inf

carbon_binned <- discretize(biomass_tr$carbon)
table(predict(carbon_binned, biomass_tr$carbon))
#> 
#> bin1 bin2 bin3 bin4 
#>  114  115  113  114 

carbon_no_infs <- discretize(biomass_tr$carbon, infs = FALSE)
predict(carbon_no_infs, c(50, 100))
#> [1] bin4 <NA>
#> Levels: bin1 bin2 bin3 bin4
```
