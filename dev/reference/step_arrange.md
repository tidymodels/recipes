# Sort rows using dplyr

`step_arrange()` creates a *specification* of a recipe step that will
sort rows using
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).

## Usage

``` r
step_arrange(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("arrange")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  Comma separated list of unquoted variable names. Use \`desc()“ to sort
  a variable in descending order. See
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- inputs:

  Quosure of values given by `...`.

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

When an object in the user's global environment is referenced in the
expression defining the new variable(s), it is a good idea to use
quasiquotation (e.g. `!!!`) to embed the value of the object in the
expression (to be portable between sessions). See the examples.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other row operation steps:
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

Other dplyr steps:
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
[`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md),
[`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
rec <- recipe(~., data = iris) |>
  step_arrange(desc(Sepal.Length), 1 / Petal.Length)

prepped <- prep(rec, training = iris |> slice(1:75))
tidy(prepped, number = 1)
#> # A tibble: 2 × 2
#>   terms              id           
#>   <chr>              <chr>        
#> 1 desc(Sepal.Length) arrange_3jx2i
#> 2 1/Petal.Length     arrange_3jx2i

library(dplyr)

dplyr_train <-
  iris |>
  as_tibble() |>
  slice(1:75) |>
  dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)

rec_train <- bake(prepped, new_data = NULL)
all.equal(dplyr_train, rec_train)
#> [1] TRUE

dplyr_test <-
  iris |>
  as_tibble() |>
  slice(76:150) |>
  dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)
rec_test <- bake(prepped, iris |> slice(76:150))
all.equal(dplyr_test, rec_test)
#> [1] TRUE

# When you have variables/expressions, you can create a
# list of symbols with `rlang::syms()`` and splice them in
# the call with `!!!`. See https://tidyeval.tidyverse.org

sort_vars <- c("Sepal.Length", "Petal.Length")

qq_rec <-
  recipe(~., data = iris) |>
  # Embed the `values` object in the call using !!!
  step_arrange(!!!syms(sort_vars)) |>
  prep(training = iris)

tidy(qq_rec, number = 1)
#> # A tibble: 2 × 2
#>   terms        id           
#>   <chr>        <chr>        
#> 1 Sepal.Length arrange_yjisy
#> 2 Petal.Length arrange_yjisy
```
