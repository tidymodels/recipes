# Create traditional dummy variables

`step_dummy()` creates a *specification* of a recipe step that will
convert nominal data (e.g. factors) into one or more numeric binary
model terms corresponding to the levels of the original data.

## Usage

``` r
step_dummy(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  one_hot = FALSE,
  contrasts = list(unordered = "contr.treatment", ordered = "contr.poly"),
  preserve = deprecated(),
  naming = dummy_names,
  levels = NULL,
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy")
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

- one_hot:

  A logical. For C levels, should C dummy variables be created rather
  than C-1?

- contrasts:

  A named vector or list of contrast functions names. Defaults to
  `list(unordered = "contr.treatment", ordered = "contr.poly")`. If only
  a single string is passed it will be used for both `unordered` and
  `ordered`.

- preserve:

  This argument has been deprecated. Please use `keep_original_cols`
  instead.

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

`step_dummy()` will create a set of binary dummy variables from a factor
variable. For example, if an unordered factor column in the data set has
levels of `"red"`, `"green"`, `"blue"`, the dummy variable bake will
create two additional columns of 0/1 data for two of those three values
(and remove the original column). For ordered factors, polynomial
contrasts are used to encode the numeric values. These defaults are
controlled by the `contrasts` argument. Note that since the contrasts
are specified via character strings you will need to have those packages
loaded. If you are using this with the tune package, you might need to
add that these packages to the `pkg` option in `control_grid()`.

By default, the excluded dummy variable (i.e. the reference cell) will
correspond to the first level of the unordered factor being converted.
[`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md)
can be used to create a new reference level by setting the `ref_level`
argument.

This recipe step allows for flexible naming of the resulting variables.
For an unordered factor named `x`, with levels `"a"` and `"b"`, the
default naming convention would be to create a new variable called
`x_b`. The naming format can be changed using the `naming` argument; the
function
[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)
is the default.

When the factor being converted has a missing value, all of the
corresponding dummy variables are also missing. See
[`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md)
for a solution.

When data to be processed contains novel levels (i.e., not contained in
the training set), a missing value is assigned to the results. See
[`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
for an alternative.

If no columns are selected (perhaps due to an earlier
[`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)),
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) will
return the data as-is (e.g. with no dummy variables).

Note that, by default, the new dummy variable column names obey the
naming rules for columns. If there are levels such as `"0"`,
[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)
will put a leading `"X"` in front of the level (since it uses
[`make.names()`](https://rdrr.io/r/base/make.names.html)). This can be
changed by passing in a different function to the `naming` argument for
this step.

Also, there are a number of contrast methods that return fractional
values. The columns returned by this step are doubles (not integers)
when `sparse = FALSE`. The columns returned when `sparse = TRUE` are
integers.

The [package vignette for dummy
variables](https://recipes.tidymodels.org/articles/Dummies.html) and
interactions has more information.

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

[`dummy_names()`](https://recipes.tidymodels.org/dev/reference/names0.md)

Other dummy variable and encoding steps:
[`step_bin2factor()`](https://recipes.tidymodels.org/dev/reference/step_bin2factor.md),
[`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
[`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
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
data(Sacramento, package = "modeldata")

# Original data: city has 37 levels
length(unique(Sacramento$city))
#> [1] 37
unique(Sacramento$city) |> sort()
#>  [1] ANTELOPE        AUBURN          CAMERON_PARK    CARMICHAEL     
#>  [5] CITRUS_HEIGHTS  COOL            DIAMOND_SPRINGS EL_DORADO      
#>  [9] EL_DORADO_HILLS ELK_GROVE       ELVERTA         FAIR_OAKS      
#> [13] FOLSOM          FORESTHILL      GALT            GARDEN_VALLEY  
#> [17] GOLD_RIVER      GRANITE_BAY     GREENWOOD       LINCOLN        
#> [21] LOOMIS          MATHER          MEADOW_VISTA    NORTH_HIGHLANDS
#> [25] ORANGEVALE      PENRYN          PLACERVILLE     POLLOCK_PINES  
#> [29] RANCHO_CORDOVA  RANCHO_MURIETA  RIO_LINDA       ROCKLIN        
#> [33] ROSEVILLE       SACRAMENTO      WALNUT_GROVE    WEST_SACRAMENTO
#> [37] WILTON         
#> 37 Levels: ANTELOPE AUBURN CAMERON_PARK CARMICHAEL ... WILTON

rec <- recipe(~ city + sqft + price, data = Sacramento)

# Default dummy coding: 36 dummy variables
dummies <- rec |>
  step_dummy(city) |>
  prep()

dummy_data <- bake(dummies, new_data = NULL)

dummy_data |>
  select(starts_with("city")) |>
  glimpse() # level "anything" is the reference level
#> Rows: 932
#> Columns: 36
#> $ city_AUBURN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CAMERON_PARK    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CARMICHAEL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CITRUS_HEIGHTS  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_COOL            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_DIAMOND_SPRINGS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO_HILLS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELK_GROVE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELVERTA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FAIR_OAKS       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FOLSOM          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FORESTHILL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GALT            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GARDEN_VALLEY   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GOLD_RIVER      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GRANITE_BAY     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GREENWOOD       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LINCOLN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LOOMIS          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MATHER          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MEADOW_VISTA    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_NORTH_HIGHLANDS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ORANGEVALE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PENRYN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PLACERVILLE     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_POLLOCK_PINES   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_RANCHO_CORDOVA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
#> $ city_RANCHO_MURIETA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_RIO_LINDA       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
#> $ city_ROCKLIN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ROSEVILLE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_SACRAMENTO      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1…
#> $ city_WALNUT_GROVE    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_WEST_SACRAMENTO <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_WILTON          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

# Obtain the full set of 37 dummy variables using `one_hot` option
dummies_one_hot <- rec |>
  step_dummy(city, one_hot = TRUE) |>
  prep()

dummy_data_one_hot <- bake(dummies_one_hot, new_data = NULL)

dummy_data_one_hot |>
  select(starts_with("city")) |>
  glimpse() # no reference level
#> Rows: 932
#> Columns: 37
#> $ city_ANTELOPE        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_AUBURN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CAMERON_PARK    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CARMICHAEL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CITRUS_HEIGHTS  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_COOL            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_DIAMOND_SPRINGS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO_HILLS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELK_GROVE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELVERTA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FAIR_OAKS       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FOLSOM          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FORESTHILL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GALT            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GARDEN_VALLEY   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GOLD_RIVER      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GRANITE_BAY     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GREENWOOD       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LINCOLN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LOOMIS          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MATHER          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MEADOW_VISTA    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_NORTH_HIGHLANDS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ORANGEVALE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PENRYN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PLACERVILLE     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_POLLOCK_PINES   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_RANCHO_CORDOVA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
#> $ city_RANCHO_MURIETA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_RIO_LINDA       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
#> $ city_ROCKLIN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ROSEVILLE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_SACRAMENTO      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1…
#> $ city_WALNUT_GROVE    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_WEST_SACRAMENTO <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_WILTON          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

# Obtain the full set of 37 dummy variables using helmert contrasts
dummies_helmert <- rec |>
  step_dummy(city, contrasts = "contr.helmert") |>
  prep()

dummy_data_helmert <- bake(dummies_helmert, new_data = NULL)

dummy_data_helmert |>
  select(starts_with("city")) |>
  glimpse() # no reference level
#> Rows: 932
#> Columns: 36
#> $ city_AUBURN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CAMERON_PARK    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CARMICHAEL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_CITRUS_HEIGHTS  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_COOL            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_DIAMOND_SPRINGS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_EL_DORADO_HILLS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELK_GROVE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ELVERTA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FAIR_OAKS       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FOLSOM          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_FORESTHILL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GALT            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GARDEN_VALLEY   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GOLD_RIVER      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GRANITE_BAY     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_GREENWOOD       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LINCOLN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_LOOMIS          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MATHER          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_MEADOW_VISTA    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_NORTH_HIGHLANDS <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_ORANGEVALE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PENRYN          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_PLACERVILLE     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_POLLOCK_PINES   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ city_RANCHO_CORDOVA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 0, …
#> $ city_RANCHO_MURIETA  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, …
#> $ city_RIO_LINDA       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, -1, 30, 0, 0, 0,…
#> $ city_ROCKLIN         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0,…
#> $ city_ROSEVILLE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0,…
#> $ city_SACRAMENTO      <dbl> 33, 33, 33, 33, 33, 33, 33, 33, -1, -1, …
#> $ city_WALNUT_GROVE    <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, …
#> $ city_WEST_SACRAMENTO <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, …
#> $ city_WILTON          <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, …

tidy(dummies, number = 1)
#> # A tibble: 36 × 3
#>    terms columns         id         
#>    <chr> <chr>           <chr>      
#>  1 city  AUBURN          dummy_qApoE
#>  2 city  CAMERON_PARK    dummy_qApoE
#>  3 city  CARMICHAEL      dummy_qApoE
#>  4 city  CITRUS_HEIGHTS  dummy_qApoE
#>  5 city  COOL            dummy_qApoE
#>  6 city  DIAMOND_SPRINGS dummy_qApoE
#>  7 city  EL_DORADO       dummy_qApoE
#>  8 city  EL_DORADO_HILLS dummy_qApoE
#>  9 city  ELK_GROVE       dummy_qApoE
#> 10 city  ELVERTA         dummy_qApoE
#> # ℹ 26 more rows
tidy(dummies_one_hot, number = 1)
#> # A tibble: 37 × 3
#>    terms columns         id         
#>    <chr> <chr>           <chr>      
#>  1 city  ANTELOPE        dummy_GyP2S
#>  2 city  AUBURN          dummy_GyP2S
#>  3 city  CAMERON_PARK    dummy_GyP2S
#>  4 city  CARMICHAEL      dummy_GyP2S
#>  5 city  CITRUS_HEIGHTS  dummy_GyP2S
#>  6 city  COOL            dummy_GyP2S
#>  7 city  DIAMOND_SPRINGS dummy_GyP2S
#>  8 city  EL_DORADO       dummy_GyP2S
#>  9 city  EL_DORADO_HILLS dummy_GyP2S
#> 10 city  ELK_GROVE       dummy_GyP2S
#> # ℹ 27 more rows
tidy(dummies_helmert, number = 1)
#> # A tibble: 36 × 3
#>    terms columns         id         
#>    <chr> <chr>           <chr>      
#>  1 city  AUBURN          dummy_lihkJ
#>  2 city  CAMERON_PARK    dummy_lihkJ
#>  3 city  CARMICHAEL      dummy_lihkJ
#>  4 city  CITRUS_HEIGHTS  dummy_lihkJ
#>  5 city  COOL            dummy_lihkJ
#>  6 city  DIAMOND_SPRINGS dummy_lihkJ
#>  7 city  EL_DORADO       dummy_lihkJ
#>  8 city  EL_DORADO_HILLS dummy_lihkJ
#>  9 city  ELK_GROVE       dummy_lihkJ
#> 10 city  ELVERTA         dummy_lihkJ
#> # ℹ 26 more rows
```
