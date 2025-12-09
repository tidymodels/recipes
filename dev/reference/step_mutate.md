# Add new variables using dplyr

`step_mutate()` creates a *specification* of a recipe step that will add
variables using
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
step_mutate(
  recipe,
  ...,
  .pkgs = character(),
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  Name-value pairs of expressions. See
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- .pkgs:

  Character vector, package names of functions used in expressions
  `...`. Should be specified if using non-base functions.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- inputs:

  Quosure(s) of `...`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

When using this flexible step, use extra care to avoid data leakage in
your preprocessing. Consider, for example, the transformation
`x = w > mean(w)`. When applied to new data or testing data, this
transformation would use the mean of `w` from the *new* data, not the
mean of `w` from the training data.

When an object in the user's global environment is referenced in the
expression defining the new variable(s), it is a good idea to use
quasiquotation (e.g. `!!`) to embed the value of the object in the
expression (to be portable between sessions). See the examples.

If a preceding step removes a column that is selected by name in
`step_mutate()`, the recipe will error when being estimated with
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  character, expression passed to
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other individual transformation steps:
[`step_BoxCox()`](https://recipes.tidymodels.org/dev/reference/step_BoxCox.md),
[`step_YeoJohnson()`](https://recipes.tidymodels.org/dev/reference/step_YeoJohnson.md),
[`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md),
[`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md),
[`step_hyperbolic()`](https://recipes.tidymodels.org/dev/reference/step_hyperbolic.md),
[`step_inverse()`](https://recipes.tidymodels.org/dev/reference/step_inverse.md),
[`step_invlogit()`](https://recipes.tidymodels.org/dev/reference/step_invlogit.md),
[`step_log()`](https://recipes.tidymodels.org/dev/reference/step_log.md),
[`step_logit()`](https://recipes.tidymodels.org/dev/reference/step_logit.md),
[`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md),
[`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md),
[`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md),
[`step_relu()`](https://recipes.tidymodels.org/dev/reference/step_relu.md),
[`step_sqrt()`](https://recipes.tidymodels.org/dev/reference/step_sqrt.md)

Other dplyr steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
[`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md),
[`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
rec <-
  recipe(~., data = iris) |>
  step_mutate(
    dbl_width = Sepal.Width * 2,
    half_length = Sepal.Length / 2
  )

prepped <- prep(rec, training = iris |> slice(1:75))

library(dplyr)

dplyr_train <-
  iris |>
  as_tibble() |>
  slice(1:75) |>
  mutate(
    dbl_width = Sepal.Width * 2,
    half_length = Sepal.Length / 2
  )

rec_train <- bake(prepped, new_data = NULL)
all.equal(dplyr_train, rec_train)
#> [1] TRUE

dplyr_test <-
  iris |>
  as_tibble() |>
  slice(76:150) |>
  mutate(
    dbl_width = Sepal.Width * 2,
    half_length = Sepal.Length / 2
  )
rec_test <- bake(prepped, iris |> slice(76:150))
all.equal(dplyr_test, rec_test)
#> [1] TRUE

# Embedding objects:
const <- 1.414

qq_rec <-
  recipe(~., data = iris) |>
  step_mutate(
    bad_approach = Sepal.Width * const,
    best_approach = Sepal.Width * !!const
  ) |>
  prep(training = iris)

bake(qq_rec, new_data = NULL, contains("appro")) |> slice(1:4)
#> # A tibble: 4 × 2
#>   bad_approach best_approach
#>          <dbl>         <dbl>
#> 1         4.95          4.95
#> 2         4.24          4.24
#> 3         4.52          4.52
#> 4         4.38          4.38

# The difference:
tidy(qq_rec, number = 1)
#> # A tibble: 2 × 3
#>   terms         value               id          
#>   <chr>         <chr>               <chr>       
#> 1 bad_approach  Sepal.Width * const mutate_5TXmN
#> 2 best_approach Sepal.Width * 1.414 mutate_5TXmN

# Using across()
recipe(~., data = iris) |>
  step_mutate(across(contains("Length"), .fns = ~ 1 / .)) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:10)
#> # A tibble: 10 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1        0.196         3.5        0.714         0.2 setosa 
#>  2        0.204         3          0.714         0.2 setosa 
#>  3        0.213         3.2        0.769         0.2 setosa 
#>  4        0.217         3.1        0.667         0.2 setosa 
#>  5        0.2           3.6        0.714         0.2 setosa 
#>  6        0.185         3.9        0.588         0.4 setosa 
#>  7        0.217         3.4        0.714         0.3 setosa 
#>  8        0.2           3.4        0.667         0.2 setosa 
#>  9        0.227         2.9        0.714         0.2 setosa 
#> 10        0.204         3.1        0.667         0.1 setosa 

recipe(~., data = iris) |>
  # leads to more columns being created.
  step_mutate(
    across(contains("Length"), .fns = list(log = log, sqrt = sqrt))
  ) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:10)
#> # A tibble: 10 × 9
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 4 more variables: Sepal.Length_log <dbl>, Sepal.Length_sqrt <dbl>,
#> #   Petal.Length_log <dbl>, Petal.Length_sqrt <dbl>
```
