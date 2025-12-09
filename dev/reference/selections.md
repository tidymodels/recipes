# Methods for selecting variables in step functions

Tips for selecting columns in step functions.

## Details

When selecting variables or model terms in `step` functions,
`dplyr`-like tools are used. The *selector* functions can choose
variables based on their name, current role, data type, or any
combination of these. The selectors are passed as any other argument to
the step. If the variables are explicitly named in the step function,
this might look like:

      recipe( ~ ., data = USArrests) %>%
        step_pca(Murder, Assault, UrbanPop, Rape, num_comp = 3)

The first four arguments indicate which variables should be used in the
PCA while the last argument is a specific argument to
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md)
about the number of components.

Note that:

1.  These arguments are not evaluated until the `prep` function for the
    step is executed.

2.  The `dplyr`-like syntax allows for negative signs to exclude
    variables (e.g. `-Murder`) and the set of selectors will processed
    in order.

3.  A leading exclusion in these arguments (e.g. `-Murder`) has the
    effect of adding *all* variables to the list except the excluded
    variable(s), ignoring role information.

Select helpers from the `tidyselect` package can also be used:
[`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`tidyselect::ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`tidyselect::contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`tidyselect::matches()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`tidyselect::num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html),
[`tidyselect::one_of()`](https://tidyselect.r-lib.org/reference/one_of.html),
[`tidyselect::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
and
[`tidyselect::any_of()`](https://tidyselect.r-lib.org/reference/all_of.html)

Note that using
[`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)
or any of the other `tidyselect` functions aren't restricted to
predictors. They will thus select outcomes, ID, and predictor columns
alike. This is why these functions should be used with care, and why
[`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)
likely isn't what you need.

For example:

      recipe(Species ~ ., data = iris) %>%
        step_center(starts_with("Sepal"), -contains("Width"))

would only select `Sepal.Length`

Columns of the design matrix that may not exist when the step is coded
can also be selected. For example, when using
[`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
the number of columns created by feature extraction may not be known
when subsequent steps are defined. In this case, using `matches("^PC")`
will select all of the columns whose names start with "PC" *once those
columns are created*.

There are sets of recipes-specific functions that can be used to select
variables based on their role or type:
[`has_role()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
and
[`has_type()`](https://recipes.tidymodels.org/dev/reference/has_role.md).
For convenience, there are also functions that are more specific. The
functions
[`all_numeric()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
and
[`all_nominal()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
select based on type, with nominal variables including both character
and factor; the functions
[`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
and
[`all_outcomes()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
select based on role. The functions
[`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
and
[`all_nominal_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
select intersections of role and type. Any can be used in conjunction
with the previous functions described for selecting variables using
their names.

A selection like this:

      data(biomass)
      recipe(HHV ~ ., data = biomass) %>%
        step_center(all_numeric(), -all_outcomes())

is equivalent to:

      data(biomass)
      recipe(HHV ~ ., data = biomass) %>%
        step_center(all_numeric_predictors())

Both result in all the numeric predictors: carbon, hydrogen, oxygen,
nitrogen, and sulfur.

If a role for a variable has not been defined, it will never be selected
using role-specific selectors.

### Interactions

Selectors can be used in
[`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md)
in similar ways but must be embedded in a model formula (as opposed to a
sequence of selectors). For example, the interaction specification could
be `~ starts_with("Species"):Sepal.Width`. This can be useful if
`Species` was converted to dummy variables previously using
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md).
The implementation of
[`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md)
is special, and is more restricted than the other step functions. Only
the selector functions from recipes and tidyselect are allowed. User
defined selector functions will not be recognized. Additionally, the
tidyselect domain specific language is not recognized here, meaning that
`&`, `|`, `!`, and `-` will not work.

### Tips for saving recipes and filtering columns

When creating variable selections:

- If you are using column filtering steps, such as
  [`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md),
  try to avoid hardcoding specific variable names in downstream steps in
  case those columns are removed by the filter. Instead, use
  [`dplyr::any_of()`](https://dplyr.tidyverse.org/reference/reexports.html)
  and
  [`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

  - [`dplyr::any_of()`](https://dplyr.tidyverse.org/reference/reexports.html)
    will be tolerant if a column has been removed.

  - [`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html)
    will fail unless all of the columns are present in the data.

- For both of these functions, if you are going to save the recipe as a
  binary object to use in another R session, try to avoid referring to a
  vector in your workspace.

  - Preferred: `any_of(!!var_names)`

  - Avoid: `any_of(var_names)`

Some examples:

    some_vars <- names(mtcars)[4:6]

    # No filter steps, OK for not saving the recipe
    rec_1 <-
      recipe(mpg ~ ., data = mtcars) |>
      step_log(all_of(some_vars)) |>
      prep()

    # No filter steps, saving the recipe
    rec_2 <-
      recipe(mpg ~ ., data = mtcars) |>
      step_log(!!!some_vars) |>
      prep()

    # This fails since `wt` is not in the data
    try(
    recipe(mpg ~ ., data = mtcars) |>
      step_rm(wt) |>
      step_log(!!!some_vars) |>
      prep(),
      silent = TRUE
    )

    # Best for filters (using any_of()) and when
    # saving the recipe
    rec_4 <-
      recipe(mpg ~ ., data = mtcars) |>
      step_rm(wt) |>
      step_log(any_of(!!some_vars)) |>
      # equal to step_log(any_of(c("hp", "drat", "wt")))
      prep()
