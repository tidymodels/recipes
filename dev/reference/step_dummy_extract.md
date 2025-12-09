# Extract patterns from nominal data

`step_dummy_extract()` creates a *specification* of a recipe step that
will convert nominal data (e.g. characters or factors) into one or more
integer model terms for the extracted levels.

## Usage

``` r
step_dummy_extract(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  sep = NULL,
  pattern = NULL,
  threshold = 0,
  other = "other",
  naming = dummy_extract_names,
  levels = NULL,
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy_extract")
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

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- sep:

  Character string containing a regular expression to use for splitting.
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) is used to
  perform the split. `sep` takes priority if `pattern` is also
  specified.

- pattern:

  Character string containing a regular expression used for extraction.
  [`gregexpr()`](https://rdrr.io/r/base/grep.html) and
  [`regmatches()`](https://rdrr.io/r/base/regmatches.html) are used to
  perform pattern extraction using `perl = TRUE`.

- threshold:

  A numeric value between 0 and 1, or an integer greater or equal to
  one. If less than one, then factor levels with a rate of occurrence in
  the training set below `threshold` will be pooled to `other`. If
  greater or equal to one, then this value is treated as a frequency and
  factor levels that occur less than `threshold` times will be pooled to
  `other`.

- other:

  A single character value for the other category, default to `"other"`.

- naming:

  A function that defines the naming convention for new dummy columns.
  See Details below.

- levels:

  A list that contains the information needed to create dummy variables
  for each variable contained in `terms`. This is `NULL` until the step
  is trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

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

`step_dummy_extract()` will create a set of integer dummy variables from
a character variable by extracting individual strings by either
splitting or extracting then counting those to create count variables.

Note that `threshold` works in a very specific way for this step. While
it is possible for one label to be present multiple times in the same
row, it will only be counted once when calculating the occurrences and
frequencies.

This recipe step allows for flexible naming of the resulting variables.
For an unordered factor named `x`, with levels `"a"` and `"b"`, the
default naming convention would be to create a new variable called
`x_b`. The naming format can be changed using the `naming` argument; the
function
[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)
is the default.

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

The return value is ordered according to the frequency of `columns`
entries in the training data set.

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

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

[`dummy_extract_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
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
data(tate_text, package = "modeldata")

dummies <- recipe(~ artist + medium, data = tate_text) |>
  step_dummy_extract(artist, medium, sep = ", ") |>
  prep()

dummy_data <- bake(dummies, new_data = NULL)

dummy_data |>
  select(starts_with("medium")) |>
  names() |>
  head()
#> [1] "medium_X1.person"                   
#> [2] "medium_X1.projection.and.1.monitor" 
#> [3] "medium_X100.digital.prints.on.paper"
#> [4] "medium_X100.works.on.paper"         
#> [5] "medium_X11.photographs"             
#> [6] "medium_X11.works.on.panel"          

# More detailed splitting
dummies_specific <- recipe(~medium, data = tate_text) |>
  step_dummy_extract(medium, sep = "(, )|( and )|( on )") |>
  prep()

dummy_data_specific <- bake(dummies_specific, new_data = NULL)

dummy_data_specific |>
  select(starts_with("medium")) |>
  names() |>
  head()
#> [1] "medium_X1.monitor"          "medium_X1.person"          
#> [3] "medium_X1.projection"       "medium_X10.light.boxes"    
#> [5] "medium_X10.tranformers"     "medium_X100.digital.prints"

tidy(dummies, number = 1)
#> # A tibble: 2,673 × 3
#>    terms  columns id                 
#>    <chr>  <chr>   <chr>              
#>  1 artist Thomas  dummy_extract_RCb4Q
#>  2 artist Schütte dummy_extract_RCb4Q
#>  3 artist John    dummy_extract_RCb4Q
#>  4 artist Akram   dummy_extract_RCb4Q
#>  5 artist Zaatari dummy_extract_RCb4Q
#>  6 artist Joseph  dummy_extract_RCb4Q
#>  7 artist Beuys   dummy_extract_RCb4Q
#>  8 artist Richard dummy_extract_RCb4Q
#>  9 artist Ferrari dummy_extract_RCb4Q
#> 10 artist León    dummy_extract_RCb4Q
#> # ℹ 2,663 more rows
tidy(dummies_specific, number = 1)
#> # A tibble: 1,216 × 3
#>    terms  columns              id                 
#>    <chr>  <chr>                <chr>              
#>  1 medium paper                dummy_extract_iz73s
#>  2 medium Etching              dummy_extract_iz73s
#>  3 medium Photograph           dummy_extract_iz73s
#>  4 medium colour               dummy_extract_iz73s
#>  5 medium gelatin silver print dummy_extract_iz73s
#>  6 medium Screenprint          dummy_extract_iz73s
#>  7 medium Lithograph           dummy_extract_iz73s
#>  8 medium on paper             dummy_extract_iz73s
#>  9 medium canvas               dummy_extract_iz73s
#> 10 medium aquatint             dummy_extract_iz73s
#> # ℹ 1,206 more rows

# pattern argument can be useful to extract harder patterns
color_examples <- tibble(
  colors = c(
    "['red', 'blue']",
    "['red', 'blue', 'white']",
    "['blue', 'blue', 'blue']"
  )
)

dummies_color <- recipe(~colors, data = color_examples) |>
  step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')") |>
  prep()

dummies_data_color <- dummies_color |>
  bake(new_data = NULL)

dummies_data_color
#> # A tibble: 3 × 4
#>   colors_blue colors_red colors_white colors_other
#>         <int>      <int>        <int>        <int>
#> 1           1          1            0            0
#> 2           1          1            1            0
#> 3           3          0            0            0
```
