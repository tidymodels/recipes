# Handle levels in multiple predictors together

`step_dummy_multi_choice()` creates a *specification* of a recipe step
that will convert multiple nominal data (e.g. characters or factors)
into one or more numeric binary model terms for the levels of the
original data.

## Usage

``` r
step_dummy_multi_choice(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  threshold = 0,
  levels = NULL,
  input = NULL,
  other = "other",
  naming = dummy_names,
  prefix = NULL,
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy_multi_choice")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details. The selected variables *must* be factors.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- threshold:

  A numeric value between 0 and 1, or an integer greater or equal to
  one. If less than one, then factor levels with a rate of occurrence in
  the training set below `threshold` will be pooled to `other`. If
  greater or equal to one, then this value is treated as a frequency and
  factor levels that occur less than `threshold` times will be pooled to
  `other`.

- levels:

  A list that contains the information needed to create dummy variables
  for each variable contained in `terms`. This is `NULL` until the step
  is trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- input:

  A character vector containing the names of the columns used. This is
  `NULL` until the step is trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- other:

  A single character value for the other category, default to `"other"`.

- naming:

  A function that defines the naming convention for new dummy columns.
  See Details below.

- prefix:

  A character string for the prefix of the resulting new variables. See
  notes below.

- sparse:

  A single string. Should the columns produced be sparse vectors. Can
  take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
  then workflows can determine the best option. Defaults to `"auto"`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

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

The overall proportion (or total counts) of the categories are computed.
The `"other"` category is used in place of any categorical levels whose
individual proportion (or frequency) in the training set is less than
`threshold`.

This step produces a number of columns, based on the number of
categories it finds. The naming of the columns is determined by the
function based on the `naming` argument. The default is to return
`<prefix>_<category name>`. By default `prefix` is `NULL`, which means
the name of the first column selected will be used in place.

This recipe step allows for flexible naming of the resulting variables.
For an unordered factor named `x`, with levels `"a"` and `"b"`, the
default naming convention would be to create a new variable called
`x_b`. The naming format can be changed using the `naming` argument; the
function
[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)
is the default.

## Tuning Parameters

This step has 1 tuning parameters:

- `threshold`: Threshold (type: double, default: 0)

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `columns` , and
`id`:

- terms:

  character, the selectors or variables selected

- columns:

  character, names of resulting columns

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
[`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md),
[`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
[`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
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
library(tibble)
languages <- tribble(
  ~lang_1,    ~lang_2,   ~lang_3,
  "English",  "Italian", NA,
  "Spanish",  NA,        "French",
  "Armenian", "English", "French",
  NA,         NA,        NA
)

dummy_multi_choice_rec <- recipe(~., data = languages) |>
  step_dummy_multi_choice(starts_with("lang")) |>
  prep()

bake(dummy_multi_choice_rec, new_data = NULL)
#> # A tibble: 4 × 5
#>   lang_1_Armenian lang_1_English lang_1_French lang_1_Italian
#>             <int>          <int>         <int>          <int>
#> 1               0              1             0              1
#> 2               0              0             1              0
#> 3               1              1             1              0
#> 4               0              0             0              0
#> # ℹ 1 more variable: lang_1_Spanish <int>
tidy(dummy_multi_choice_rec, number = 1)
#> # A tibble: 5 × 3
#>   terms  columns  id                      
#>   <chr>  <chr>    <chr>                   
#> 1 lang_1 Armenian dummy_multi_choice_sZ4HI
#> 2 lang_1 English  dummy_multi_choice_sZ4HI
#> 3 lang_1 French   dummy_multi_choice_sZ4HI
#> 4 lang_1 Italian  dummy_multi_choice_sZ4HI
#> 5 lang_1 Spanish  dummy_multi_choice_sZ4HI

dummy_multi_choice_rec2 <- recipe(~., data = languages) |>
  step_dummy_multi_choice(starts_with("lang"),
    prefix = "lang",
    threshold = 0.2
  ) |>
  prep()

bake(dummy_multi_choice_rec2, new_data = NULL)
#> # A tibble: 4 × 2
#>   lang_English lang_other
#>          <int>      <int>
#> 1            1          1
#> 2            0          1
#> 3            1          1
#> 4            0          0
tidy(dummy_multi_choice_rec2, number = 1)
#> # A tibble: 2 × 3
#>   terms  columns id                      
#>   <chr>  <chr>   <chr>                   
#> 1 lang_1 English dummy_multi_choice_YaDdR
#> 2 lang_1 other   dummy_multi_choice_YaDdR
```
