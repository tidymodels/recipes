# Filter rows using dplyr

`step_filter()` creates a *specification* of a recipe step that will
remove rows using
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Usage

``` r
step_filter(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = TRUE,
  id = rand_id("filter")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  Logical predicates defined in terms of the variables in the data.
  Multiple conditions are combined with `&`. Only rows where the
  condition evaluates to `TRUE` are kept. See
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
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
  `skip = FALSE`.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

When an object in the user's global environment is referenced in the
expression defining the new variable(s), it is a good idea to use
quasiquotation (e.g. `!!`) to embed the value of the object in the
expression (to be portable between sessions). See the examples.

## Row Filtering

This step can entirely remove observations (rows of data), which can
have unintended and/or problematic consequences when applying the step
to new data later via
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
Consider whether `skip = TRUE` or `skip = FALSE` is more appropriate in
any given use case. In most instances that affect the rows of the data
being predicted, this step probably should not be applied at all;
instead, execute operations like this outside and before starting a
preprocessing
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

The expressions in `terms` are text representations and are not
parsable.

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

Other dplyr steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
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
  step_filter(Sepal.Length > 4.5, Species == "setosa")

prepped <- prep(rec, training = iris |> slice(1:75))

library(dplyr)

dplyr_train <-
  iris |>
  as_tibble() |>
  slice(1:75) |>
  dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

rec_train <- bake(prepped, new_data = NULL)
all.equal(dplyr_train, rec_train)
#> [1] TRUE

dplyr_test <-
  iris |>
  as_tibble() |>
  slice(76:150) |>
  dplyr::filter(Sepal.Length > 4.5, Species != "setosa")
rec_test <- bake(prepped, iris |> slice(76:150))
all.equal(dplyr_test, rec_test)
#> [1] TRUE

values <- c("versicolor", "virginica")

qq_rec <-
  recipe(~., data = iris) |>
  # Embed the `values` object in the call using !!
  step_filter(Sepal.Length > 4.5, Species %in% !!values)

tidy(qq_rec, number = 1)
#> # A tibble: 2 × 2
#>   terms                                           id          
#>   <chr>                                           <chr>       
#> 1 "Sepal.Length > 4.5"                            filter_UD7hV
#> 2 "Species %in% c(\"versicolor\", \"virginica\")" filter_UD7hV
```
