# Create counts of patterns using regular expressions

`step_count()` creates a *specification* of a recipe step that will
create a variable that counts instances of a regular expression pattern
in text.

## Usage

``` r
step_count(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  pattern = ".",
  normalize = FALSE,
  options = list(),
  result = make.names(pattern),
  input = NULL,
  sparse = "auto",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("count")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  A single selector function to choose which variable will be searched
  for the regex pattern. The selector should resolve to a single
  variable. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- pattern:

  A character string containing a regular expression (or character
  string for `fixed = TRUE`) to be matched in the given character
  vector. Coerced by `as.character` to a character string if possible.

- normalize:

  A logical; should the integer counts be divided by the total number of
  characters in the string?.

- options:

  A list of options to [`gregexpr()`](https://rdrr.io/r/base/grep.html)
  that should not include `x` or `pattern`.

- result:

  A single character value for the name of the new variable. It should
  be a valid column name.

- input:

  A single character value for the name of the variable being searched.
  This is `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

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
this step, a tibble is returned with columns `terms`, `result` , and
`id`:

- terms:

  character, the selectors or variables selected

- result:

  character, the new column names

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
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
[`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md),
[`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md),
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
data(covers, package = "modeldata")

rec <- recipe(~description, covers) |>
  step_count(description, pattern = "(rock|stony)", result = "rocks") |>
  step_count(description, pattern = "famil", normalize = TRUE)

rec2 <- prep(rec, training = covers)
rec2
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> predictor: 1
#> 
#> ── Training information 
#> Training data contained 40 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Regular expression counts using: description | Trained
#> • Regular expression counts using: description | Trained

count_values <- bake(rec2, new_data = covers)
count_values
#> # A tibble: 40 × 3
#>    description                                             rocks  famil
#>    <fct>                                                   <int>  <dbl>
#>  1 1,cathedral family,rock outcrop complex,extremely stony     2 0.0182
#>  2 2,vanet,ratake families complex,very stony                  1 0.0238
#>  3 3,haploborolis,rock outcrop complex,rubbly                  1 0     
#>  4 4,ratake family,rock outcrop complex,rubbly                 1 0.0233
#>  5 5,vanet family,rock outcrop complex complex,rubbly          1 0.02  
#>  6 6,vanet,wetmore families,rock outcrop complex,stony         2 0.0196
#>  7 7,gothic family                                             0 0.0667
#>  8 8,supervisor,limber families complex                        0 0.0278
#>  9 9,troutville family,very stony                              1 0.0333
#> 10 10,bullwark,catamount families,rock outcrop complex,ru…     1 0.0172
#> # ℹ 30 more rows

tidy(rec, number = 1)
#> # A tibble: 1 × 3
#>   terms       result id         
#>   <chr>       <chr>  <chr>      
#> 1 description NA     count_7KJwk
tidy(rec2, number = 1)
#> # A tibble: 1 × 3
#>   terms       result id         
#>   <chr>       <chr>  <chr>      
#> 1 description rocks  count_7KJwk
```
