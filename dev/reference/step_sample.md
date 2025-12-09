# Sample rows using dplyr

`step_sample()` creates a *specification* of a recipe step that will
sample rows using
[`dplyr::sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html)
or
[`dplyr::sample_frac()`](https://dplyr.tidyverse.org/reference/sample_n.html).

## Usage

``` r
step_sample(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  size = NULL,
  replace = FALSE,
  skip = TRUE,
  id = rand_id("sample")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  Argument ignored; included for consistency with other step
  specification functions.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- size:

  An integer or fraction. If the value is within (0, 1),
  [`dplyr::sample_frac()`](https://dplyr.tidyverse.org/reference/sample_n.html)
  is applied to the data. If an integer value of 1 or greater is used,
  [`dplyr::sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html)
  is applied. The default of `NULL` uses
  [`dplyr::sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html)
  with the size of the training set (or smaller for smaller `new_data`).

- replace:

  Sample with or without replacement?

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
this step, a tibble is returned with columns `terms`, `size`, `replace`
, and `id`:

- terms:

  character, the selectors or variables selected

- size:

  numeric, amount of sampling

- replace:

  logical, whether sampling is done with replacement

- id:

  character, id of this step

## Sparse data

This step can be applied to
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
such that it is preserved. Nothing needs to be done for this to happen
as it is done automatically.

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other row operation steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md),
[`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
[`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
[`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

Other dplyr steps:
[`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md),
[`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
[`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md),
[`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
[`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md),
[`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md),
[`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md),
[`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)

## Examples

``` r
# Uses `sample_n`
recipe(~., data = mtcars) |>
  step_sample(size = 1) |>
  prep(training = mtcars) |>
  bake(new_data = NULL) |>
  nrow()
#> [1] 1

# Uses `sample_frac`
recipe(~., data = mtcars) |>
  step_sample(size = 0.9999) |>
  prep(training = mtcars) |>
  bake(new_data = NULL) |>
  nrow()
#> [1] 32

# Uses `sample_n` and returns _at maximum_ 20 samples.
smaller_cars <-
  recipe(~., data = mtcars) |>
  step_sample() |>
  prep(training = mtcars |> slice(1:20))

bake(smaller_cars, new_data = NULL) |> nrow()
#> [1] 20
bake(smaller_cars, new_data = mtcars |> slice(21:32)) |> nrow()
#> [1] 12
```
