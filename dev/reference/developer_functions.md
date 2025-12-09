# Developer functions for creating recipes steps

This page provides a comprehensive list of the exported functions for
creating recipes steps and guidance on how to use them.

## Creating steps

[`add_step()`](https://recipes.tidymodels.org/dev/reference/add_step.md)
and
[`add_check()`](https://recipes.tidymodels.org/dev/reference/add_step.md)
are required when creating a new step. The output of
[`add_step()`](https://recipes.tidymodels.org/dev/reference/add_step.md)
should be the return value of all steps and should have the following
format:

    step_example <- function(recipe,
                             ...,
                             role = NA,
                             trained = FALSE,
                             skip = FALSE,
                             id = rand_id("example")) {
      add_step(
        recipe,
        step_example_new(
          terms = enquos(...),
          role = role,
          trained = trained,
          skip = skip,
          id = id
        )
      )
    }

[`rand_id()`](https://recipes.tidymodels.org/dev/reference/rand_id.md)
should be used in the arguments of `step_example()` to specify the
argument, as we see in the above example.

[`recipes_pkg_check()`](https://recipes.tidymodels.org/dev/reference/recipes_pkg_check.md)
should be used in `step_example()` functions together with
[`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
to alert users that certain other packages are required. The standard
way of using this function is the following format:

    recipes_pkg_check(required_pkgs.step_example())

[`step()`](https://recipes.tidymodels.org/dev/reference/step.md) and
[`check()`](https://recipes.tidymodels.org/dev/reference/step.md) are
used within the `step_*_new()` function that you use in your new step.
It will be used in the following way:

    step_example_new <- function(terms, role, trained, skip, id) {
      step(
        subclass = "example",
        terms = terms,
        role = role,
        trained = trained,
        skip = skip,
        id = id
      )
    }

[`recipes_eval_select()`](https://recipes.tidymodels.org/dev/reference/recipes_eval_select.md)
is used within `prep.step_*()` functions, and are used to turn the
`terms` object into a character vector of the selected variables.

It will most likely be used like so:

    col_names <- recipes_eval_select(x$terms, training, info)

[`recipes_argument_select()`](https://recipes.tidymodels.org/dev/reference/recipes_argument_select.md)
is used within `prep.step_*()` functions in the same way as
[`recipes_eval_select()`](https://recipes.tidymodels.org/dev/reference/recipes_eval_select.md)
but is intended to be used for arguments such as `denom` in
[`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md).

It will most likely be used like so:

    outcome_var <- recipes_argument_select(x$outcome, training, info)

[`check_type()`](https://recipes.tidymodels.org/dev/reference/check_type.md)
can be used within `prep.step_*()` functions to check that the variables
passed in are the right types. We recommend that you use the `types`
argument as it offers higher flexibility and it matches the types
defined by
[`.get_data_types()`](https://recipes.tidymodels.org/dev/reference/get_data_types.md).
When using `types` we find it better to be explicit, e.g. writing
`types = c("double", "integer")` instead of `types = "numeric"`, as it
produces cleaner error messages.

It should be used like so:

    check_type(training[, col_names], types = c("double", "integer"))

[`check_options()`](https://recipes.tidymodels.org/dev/reference/check_options.md)
can be used within `prep.step_*()` functions to check that the `options`
argument contains the right elements. It doens't check the types of the
elements, just that `options` is a named list and it includes or
excludes some names.

It should be used like so:

    # When you know some arguments are excluded
    check_options(xoptions, exclude = c("x", "pattern"))

    # When you know all legal elements
    check_options(xoptions, include = c("nthread", "eps"))

[`check_new_data()`](https://recipes.tidymodels.org/dev/reference/check_new_data.md)
should be used within `bake.step_*()`. This function is used to make
check that the required columns are present in the data. It should be
one of the first lines inside the function.

It should be used like so:

    check_new_data(names(object$columns), object, new_data)

[`check_name()`](https://recipes.tidymodels.org/dev/reference/check_name.md)
should be used in `bake.step_*()` functions for steps that add new
columns to the data set. The function throws an error if the column
names already exist in the data set. It should be called before adding
the new columns to the data set.

[`get_keep_original_cols()`](https://recipes.tidymodels.org/dev/reference/get_keep_original_cols.md)
and
[`remove_original_cols()`](https://recipes.tidymodels.org/dev/reference/remove_original_cols.md)
are used within steps with the `keep_original_cols` argument.
[`get_keep_original_cols()`](https://recipes.tidymodels.org/dev/reference/get_keep_original_cols.md)
is used in `prep.step_*()` functions for steps that were created before
the `keep_original_cols` argument was added, and acts as a way to throw
a warning that the user should regenerate the recipe.
[`remove_original_cols()`](https://recipes.tidymodels.org/dev/reference/remove_original_cols.md)
should be used in `bake.step_*()` functions to remove the original
columns. It is worth noting that
[`remove_original_cols()`](https://recipes.tidymodels.org/dev/reference/remove_original_cols.md)
can remove multiple columns at once and when possible should be put
outside `for` loops.

    new_data <- remove_original_cols(new_data, object, names_of_original_cols)

[`recipes_remove_cols()`](https://recipes.tidymodels.org/dev/reference/recipes_remove_cols.md)
should be used in `prep.step_*()` functions, and is used to remove
columns from the data set, either by using the `object$removals` field
or by using the `col_names` argument.

[`recipes_names_predictors()`](https://recipes.tidymodels.org/dev/reference/recipes-role-indicator.md)
and
[`recipes_names_outcomes()`](https://recipes.tidymodels.org/dev/reference/recipes-role-indicator.md)
should be used in `prep.step_*()` functions, and are used to get names
of predictors and outcomes.

[`get_case_weights()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md)
and
[`are_weights_used()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md)
are functions that help you extract case weights and help determine if
they are used or not within the step. They will typically be used within
the `prep.step_*()` functions if the step in question supports case
weights.

[`print_step()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
is used inside `print.step_*()` functions. This function is replacing
the internally deprecated
[`printer()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
function.

[`sel2char()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
is mostly used within `tidy.step_*()` functions to turn selections into
character vectors.

[`names0()`](https://recipes.tidymodels.org/dev/reference/names0.md)
creates a series of `num` names with a common prefix. The names are
numbered with leading zeros (e.g. `prefix01`-`prefix10` instead of
`prefix1`-`prefix10`). This is useful for many types of steps that
produce new columns.

## Interacting with recipe objects

[`recipes_ptype()`](https://recipes.tidymodels.org/dev/reference/recipes_ptype.md)
returns the ptype, expected variables and types, that a recipe object
expects at
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time.
Controlled using the `stage` argument. Can be used by functions that
interact with recipes to verify data is correct before passing it to
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).

[`recipes_ptype_validate()`](https://recipes.tidymodels.org/dev/reference/recipes_ptype_validate.md)
checks that a recipe and its data are compatible using information
extracted using
[`recipes_ptype()`](https://recipes.tidymodels.org/dev/reference/recipes_ptype.md).
Can be used by functions that interact with recipes to verify data is
correct before passing it to
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).

[`detect_step()`](https://recipes.tidymodels.org/dev/reference/detect_step.md)
returns a logical indicator to determine if a given step or check is
included in a recipe.

[`fully_trained()`](https://recipes.tidymodels.org/dev/reference/fully_trained.md)
returns a logical indicator if the recipe is fully trained. The function
[`is_trained()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
can be used to check in any individual steps are trained or not.

[`.get_data_types()`](https://recipes.tidymodels.org/dev/reference/get_data_types.md)
is an S3 method that is used for
[selections](https://recipes.tidymodels.org/dev/reference/selections.md).
This method can be extended to work with column types not supported by
recipes.

[`recipes_extension_check()`](https://recipes.tidymodels.org/dev/reference/recipes_extension_check.md)
is recommended to be used by package authors to make sure that all steps
have `prep.step_*()`, `bake.step_*()`, `print.step_*()`,
`tidy.step_*()`, and `required_pkgs.step_*()` methods. It should be used
as a test, preferably like this:

    test_that("recipes_extension_check", {
      expect_snapshot(
        recipes::recipes_extension_check(
          pkg = "pkgname"
        )
      )
    })
