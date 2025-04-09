#' @name developer_functions
#'
#' @title Developer functions for creating recipes steps
#'
#' @description
#'
#' This page provides a comprehensive list of the exported functions for
#' creating recipes steps and guidance on how to use them.
#'
#' @details
#'
#' # Creating steps
#'
#' [add_step()] and [add_check()] are required when creating a new step. The
#' output of [add_step()] should be the return value of all steps and should
#' have the following format:
#'
#' ```r
#' step_example <- function(recipe,
#'                          ...,
#'                          role = NA,
#'                          trained = FALSE,
#'                          skip = FALSE,
#'                          id = rand_id("example")) {
#'   add_step(
#'     recipe,
#'     step_example_new(
#'       terms = enquos(...),
#'       role = role,
#'       trained = trained,
#'       skip = skip,
#'       id = id
#'     )
#'   )
#' }
#' ```
#'
#' [rand_id()] should be used in the arguments of `step_example()` to specify
#' the argument, as we see in the above example.
#'
#' [recipes_pkg_check()] should be used in `step_example()` functions together
#' with [required_pkgs()] to alert users that certain other packages are
#' required. The standard way of using this function is the following format:
#'
#' ```r
#' recipes_pkg_check(required_pkgs.step_example())
#' ```
#'
#' [step()] and [check()] are used within the `step_*_new()` function that you
#' use in your new step. It will be used in the following way:
#'
#' ```r
#' step_example_new <- function(terms, role, trained, skip, id) {
#'   step(
#'     subclass = "example",
#'     terms = terms,
#'     role = role,
#'     trained = trained,
#'     skip = skip,
#'     id = id
#'   )
#' }
#' ```
#'
#' [recipes_eval_select()] is used within `prep.step_*()` functions, and are
#' used to turn the `terms` object into a character vector of the selected
#' variables.
#'
#' It will most likely be used like so:
#'
#' ```r
#' col_names <- recipes_eval_select(x$terms, training, info)
#' ```
#'
#' [recipes_argument_select()] is used within `prep.step_*()` functions in the
#' same way as [recipes_eval_select()] but is intended to be used for arguments
#' such as `denom` in [step_ratio()].
#'
#' It will most likely be used like so:
#'
#' ```r
#' outcome_var <- recipes_argument_select(x$outcome, training, info)
#' ```
#'
#' [check_type()] can be used within `prep.step_*()` functions to check that the
#' variables passed in are the right types. We recommend that you use the
#' `types` argument as it offers higher flexibility and it matches the types
#' defined by [.get_data_types()]. When using `types` we find it better to be
#' explicit, e.g. writing `types = c("double", "integer")` instead of `types =
#' "numeric"`, as it produces cleaner error messages.
#'
#' It should be used like so:
#'
#' ```r
#' check_type(training[, col_names], types = c("double", "integer"))
#' ```
#'
#' [check_options()] can be used within `prep.step_*()` functions to check that
#' the `options` argument contains the right elements. It doens't check the
#' types of the elements, just that `options` is a named list and it includes
#' or excludes some names.
#'
#' It should be used like so:
#'
#' ```r
#' # When you know some arguments are excluded
#' check_options(xoptions, exclude = c("x", "pattern"))
#'
#' # When you know all legal elements
#' check_options(xoptions, include = c("nthread", "eps"))
#' ```
#'
#' [check_new_data()] should be used within `bake.step_*()`. This function is
#' used to make check that the required columns are present in the data. It
#' should be one of the first lines inside the function.
#'
#' It should be used like so:
#'
#' ```r
#' check_new_data(names(object$columns), object, new_data)
#' ```
#'
#' [check_name()] should be used in `bake.step_*()` functions for steps that add
#' new columns to the data set. The function throws an error if the column names
#' already exist in the data set. It should be called before adding the new
#' columns to the data set.
#'
#' [get_keep_original_cols()] and [remove_original_cols()] are used within steps
#' with the `keep_original_cols` argument. [get_keep_original_cols()] is used in
#' `prep.step_*()` functions for steps that were created before the
#' `keep_original_cols` argument was added, and acts as a way to throw a warning
#' that the user should regenerate the recipe. [remove_original_cols()] should
#' be used in `bake.step_*()` functions to remove the original columns. It is
#' worth noting that [remove_original_cols()] can remove multiple columns at
#' once and when possible should be put outside `for` loops.
#'
#' ```r
#' new_data <- remove_original_cols(new_data, object, names_of_original_cols)
#' ```
#'
#' [recipes_remove_cols()] should be used in `prep.step_*()` functions, and is
#' used to remove columns from the data set, either by using the
#' `object$removals` field or by using the `col_names` argument.
#'
#' [recipes_names_predictors()] and [recipes_names_outcomes()] should be used in
#' `prep.step_*()` functions, and are used to get names of predictors and
#' outcomes.
#'
#' [get_case_weights()] and [are_weights_used()] are functions that help you
#' extract case weights and help determine if they are used or not within the
#' step. They will typically be used within the `prep.step_*()` functions if the
#' step in question supports case weights.
#'
#' [print_step()] is used inside `print.step_*()` functions. This function is
#' replacing the internally deprecated [printer()] function.
#'
#' [sel2char()] is mostly used within `tidy.step_*()` functions to turn
#' selections into character vectors.
#'
#' [names0()] creates a series of `num` names with a common prefix. The names
#' are numbered with leading zeros (e.g. `prefix01`-`prefix10` instead of
#' `prefix1`-`prefix10`). This is useful for many types of steps that produce
#' new columns.
#'
#' # Interacting with recipe objects
#'
#' [recipes_ptype()] returns the ptype, expected variables and types, that a
#' recipe object expects at `prep()` and `bake()` time. Controlled using the
#' `stage` argument. Can be used by functions that interact with recipes to
#' verify data is correct before passing it to `prep()` and `bake()`.
#'
#' [recipes_ptype_validate()] checks that a recipe and its data are compatible
#' using information extracted using [recipes_ptype()]. Can be used by functions
#' that interact with recipes to verify data is correct before passing it to
#' `prep()` and `bake()`.
#'
#' [detect_step()] returns a logical indicator to determine if a given step or
#' check is included in a recipe.
#'
#' [fully_trained()] returns a logical indicator if the recipe is fully trained.
#' The function [is_trained()] can be used to check in any individual steps are
#' trained or not.
#'
#' [.get_data_types()] is an S3 method that is used for [selections]. This method
#' can be extended to work with column types not supported by recipes.
#'
#' [recipes_extension_check()] is recommended to be used by package authors to
#' make sure that all steps have `prep.step_*()`, `bake.step_*()`,
#' `print.step_*()`, `tidy.step_*()`, and `required_pkgs.step_*()` methods. It
#' should be used as a test, preferably like this:
#'
#' ```r
#' test_that("recipes_extension_check", {
#'   expect_snapshot(
#'     recipes::recipes_extension_check(
#'       pkg = "pkgname"
#'     )
#'   )
#' })
#' ```
NULL
