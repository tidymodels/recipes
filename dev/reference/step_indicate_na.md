# Create missing data column indicators

`step_indicate_na()` creates a *specification* of a recipe step that
will create and append additional binary columns to the data set to
indicate which observations are missing.

## Usage

``` r
step_indicate_na(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  prefix = "na_ind",
  sparse = "auto",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("indicate_na")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  used.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. Defaults to "na_ind".

- sparse:

  A single string. Should the columns produced be sparse vectors. Can
  take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
  then workflows can determine the best option. Defaults to `"auto"`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `TRUE`.

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Sparse data

This step produces sparse columns if `sparse = "yes"` is being set. The
default value `"auto"` won't trigger production fo sparse columns if a
recipe is
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)ed, but
allows for a workflow to toggle to `"yes"` or `"no"` depending on
whether the model supports
[sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
and if the model is is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't
perfect, and there will be times when you want to manually overwrite by
setting `sparse = "yes"` or `sparse = "no"`.

## Case weights

The underlying operation does not allow for case weights.

## See also

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
[`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md),
[`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md),
[`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md),
[`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
[`step_integer()`](https://recipes.tidymodels.org/dev/reference/step_integer.md),
[`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md),
[`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md),
[`step_ordinalscore()`](https://recipes.tidymodels.org/dev/reference/step_ordinalscore.md),
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md),
[`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md),
[`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md),
[`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md),
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md),
[`step_unorder()`](https://recipes.tidymodels.org/dev/reference/step_unorder.md)

## Examples

``` r
data("credit_data", package = "modeldata")

## missing data per column
purrr::map_dbl(credit_data, function(x) mean(is.na(x)))
#>       Status    Seniority         Home         Time          Age 
#> 0.0000000000 0.0000000000 0.0013471037 0.0000000000 0.0000000000 
#>      Marital      Records          Job     Expenses       Income 
#> 0.0002245173 0.0000000000 0.0004490346 0.0000000000 0.0855410867 
#>       Assets         Debt       Amount        Price 
#> 0.0105523125 0.0040413112 0.0000000000 0.0000000000 

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]

rec <- recipe(Price ~ ., data = credit_tr)

impute_rec <- rec |>
  step_indicate_na(Income, Assets, Debt)

imp_models <- prep(impute_rec, training = credit_tr)

imputed_te <- bake(imp_models, new_data = credit_te)
```
