# Wrapper function for preparing recipes within resampling

When working with the rsample package, a simple recipe must be
*prepared* using the `prep` function first. When using recipes with
rsample it is helpful to have a function that can prepare a recipe
across a series of `split` objects that are produced in this package.
`prepper` is a wrapper function around
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) that
can be used to do this. See the vignette on "Recipes and rsample" for an
example.

## Usage

``` r
prepper(split_obj, recipe, ...)
```

## Arguments

- split_obj:

  An `rplit` object

- recipe:

  An untrained `recipe` object.

- ...:

  Arguments to pass to `prep` such as `verbose` or `retain`.

## Details

`prepper()` sets the underlying
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
argument `fresh` to `TRUE`.
