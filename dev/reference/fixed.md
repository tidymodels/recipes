# Helper Functions for Profile Data Sets

Helper Functions for Profile Data Sets

## Usage

``` r
fixed(x, pct, index, ...)

# Default S3 method
fixed(x, pct, index, ...)

# S3 method for class 'numeric'
fixed(x, pct, index, ...)

# S3 method for class 'factor'
fixed(x, pct, index, ...)

# S3 method for class 'character'
fixed(x, pct, index, ...)

# S3 method for class 'Date'
fixed(x, pct, index, ...)

# S3 method for class 'POSIXct'
fixed(x, pct, index, ...)

prof(x, grid, ...)

# S3 method for class 'numeric'
prof(x, grid, ...)

# S3 method for class 'factor'
prof(x, grid, ...)

# S3 method for class 'character'
prof(x, grid, ...)

# S3 method for class 'Date'
prof(x, grid, ...)

# S3 method for class 'POSIXct'
prof(x, grid, ...)
```

## Arguments

- x:

  A vector

- pct, index, ..., grid:

  Options pass from
  [`step_profile()`](https://recipes.tidymodels.org/dev/reference/step_profile.md)
