# Estimate sparsity of a recipe

Estimate sparsity of a recipe

## Usage

``` r
.recipes_estimate_sparsity(x, ...)
```

## Arguments

- x:

  An object.

## Value

A recipe

## Details

Takes a untrained recipe an provides a rough estimate of the sparsity of
the prepped version of the recipe.

Sampling of the input is done to avoid slowdown for larger data sets.

An estimated sparsity of the input data is calculated. Then each step
where `sparse = "auto"` or `sparse = "yes"` is set, an estimate of how
many predictors will be created and used to modify the estimate.

An initial sparsity of 0 will be used if a zero-row data frame is used
in specification of recipe. This is likely a under-estimate of the true
sparsity of the data.
