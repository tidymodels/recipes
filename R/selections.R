#' @name selections
#' @aliases selections
#' @aliases selection
#'
#' @title Methods for selecting variables in step functions
#'
#' @description
#'
#' Tips for selecting columns in step functions.
#'
#' @details
#'  When selecting variables or model terms in `step`
#'  functions, `dplyr`-like tools are used. The *selector* functions
#'  can choose variables based on their name, current role, data
#'  type, or any combination of these. The selectors are passed as
#'  any other argument to the step. If the variables are explicitly
#'  named in the step function, this might look like:
#'
#' \preformatted{
#'   recipe( ~ ., data = USArrests) \%>\%
#'     step_pca(Murder, Assault, UrbanPop, Rape, num_comp = 3)
#' }
#'
#'  The first four arguments indicate which variables should be
#'  used in the PCA while the last argument is a specific argument
#'  to [step_pca()] about the number of components.
#'
#' Note that:
#'
#'   \enumerate{
#'   \item These arguments are not evaluated until the `prep`
#'    function for the step is executed.
#'   \item The `dplyr`-like syntax allows for negative signs to
#'    exclude variables (e.g. `-Murder`) and the set of selectors will
#'    processed in order.
#'   \item A leading exclusion in these arguments (e.g. `-Murder`)
#'   has the effect of adding *all* variables to the list except the
#'   excluded variable(s), ignoring role information.
#'   }
#'
#' Select helpers from the `tidyselect` package can also be used:
#'   [tidyselect::starts_with()], [tidyselect::ends_with()],
#'   [tidyselect::contains()], [tidyselect::matches()],
#'   [tidyselect::num_range()], [tidyselect::everything()],
#'   [tidyselect::one_of()], [tidyselect::all_of()], and
#'   [tidyselect::any_of()]
#'
#' For example:
#'
#' \preformatted{
#'   recipe(Species ~ ., data = iris) \%>\%
#'     step_center(starts_with("Sepal"), -contains("Width"))
#' }
#'
#' would only select `Sepal.Length`
#'
#' Columns of the design matrix that may not exist when the step
#' is coded can also be selected. For example, when using
#' `step_pca()`, the number of columns created by feature extraction
#' may not be known when subsequent steps are defined. In this
#' case, using `matches("^PC")` will select all of the columns
#' whose names start with "PC" *once those columns are created*.
#'
#' There are sets of recipes-specific functions that can be used to select
#' variables based on their role or type: [has_role()] and
#' [has_type()]. For convenience, there are also functions that are
#' more specific. The functions [all_numeric()] and [all_nominal()] select
#' based on type, with nominal variables including both character and factor;
#' the functions [all_predictors()] and [all_outcomes()] select based on role.
#' The functions [all_numeric_predictors()] and [all_nominal_predictors()]
#' select intersections of role and type. Any can be used in conjunction with
#' the previous functions described for selecting variables using their names.
#'
#' A selection like this:
#'
#' \preformatted{
#'   data(biomass)
#'   recipe(HHV ~ ., data = biomass) \%>\%
#'     step_center(all_numeric(), -all_outcomes())
#' }
#'
#' is equivalent to:
#'
#' \preformatted{
#'   data(biomass)
#'   recipe(HHV ~ ., data = biomass) \%>\%
#'     step_center(all_numeric_predictors())
#' }
#'
#' Both result in all the numeric predictors: carbon, hydrogen,
#' oxygen, nitrogen, and sulfur.
#'
#' If a role for a variable has not been defined, it will never be
#' selected using role-specific selectors.
#'
#' ## Interactions
#'
#' Selectors can be used in [step_interact()] in similar ways but
#' must be embedded in a model formula (as opposed to a sequence
#' of selectors). For example, the interaction specification
#' could be `~ starts_with("Species"):Sepal.Width`. This can be
#' useful if `Species` was converted to dummy variables
#' previously using [step_dummy()]. The implementation of
#' `step_interact()` is special, and is more restricted than
#' the other step functions. Only the selector functions from
#' recipes and tidyselect are allowed. User defined selector functions
#' will not be recognized. Additionally, the tidyselect domain specific
#' language is not recognized here, meaning that `&`, `|`, `!`, and `-`
#' will not work.
#'
#' @includeRmd man/rmd/selections.Rmd details
NULL

# ------------------------------------------------------------------------------

#' Evaluate a selection with tidyselect semantics specific to recipes
#'
#' @description
#' `recipes_eval_select()` is a recipes specific variant of
#' [tidyselect::eval_select()] enhanced with the ability to recognize recipes
#' selectors, such as [all_numeric_predictors()]. See [selections]
#' for more information about the unique recipes selectors.
#'
#' This is a developer tool that is only useful for creating new recipes steps.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param quos A list of quosures describing the selection. This is generally
#'   the `...` argument of your step function, captured with [rlang::enquos()]
#'   and stored in the step object as the `terms` element.
#'
#' @param data A data frame to use as the context to evaluate the selection in.
#'   This is generally the `training` data passed to the [prep()] method
#'   of your step.
#'
#' @param info A data frame of term information describing each column's type
#'   and role for use with the recipes selectors. This is generally the `info`
#'   data passed to the [prep()] method of your step.
#'
#' @param allow_rename Should the renaming syntax `c(foo = bar)` be allowed?
#'   This is rarely required, and is currently only used by [step_select()].
#'   It is unlikely that your step will need renaming capabilities.
#'
#' @return
#' A named character vector containing the evaluated selection. The names are
#' always the same as the values, except when `allow_rename = TRUE`, in which
#' case the names reflect the new names chosen by the user.
#'
#' @export
#' @examples
#' library(rlang)
#' library(modeldata)
#' data(scat)
#'
#' rec <- recipe(Species ~ ., data = scat)
#'
#' info <- summary(rec)
#' info
#'
#' quos <- quos(all_numeric_predictors(), where(is.factor))
#'
#' recipes_eval_select(quos, scat, info)
recipes_eval_select <- function(quos, data, info, ..., allow_rename = FALSE) {
  ellipsis::check_dots_empty()

  # Maintain ordering between `data` column names and `info$variable` so
  # `eval_select()` and recipes selectors return compatible positions
  data_info <- tibble(variable = names(data))
  data_info <- dplyr::left_join(data_info, info, by = "variable")

  nested_info <- tidyr::nest(data_info, data = -variable)

  local_current_info(nested_info)

  expr <- expr(c(!!!quos))

  # FIXME: Ideally this is `FALSE` for strict selection,
  # but empty selections incorrectly throw an
  # error when this is false due to the following bug:
  # https://github.com/r-lib/tidyselect/issues/221
  # Once it's fixed, remove this and pass allow_rename to
  # tidyselect::eval_select().
  allow_rename_compat <- TRUE

  sel <- tidyselect::eval_select(
    expr = expr,
    data = data,
    allow_rename = allow_rename_compat
  )

  # Return names not positions, as these names are
  # used for both the training and test set and their positions
  # may have changed. If renaming is allowed, add the new names.
  out <- names(data)[sel]
  names <- names(sel)

  # FIXME: Remove this check when the following issue is fixed,
  # at that point, just pass `allow_rename` to `eval_select()` directly.
  # https://github.com/r-lib/tidyselect/issues/221
  if (!allow_rename & !identical(out, names)) {
    abort("Can't rename variables in this context.")
  }

  names(out) <- names
  out
}

#' Role Selection
#'
#' @description
#'
#' `has_role()`, `all_predictors()`, and `all_outcomes()` can be used to
#'  select variables in a formula that have certain roles.
#'
#' Similarly, `has_type()`, `all_numeric()`, and `all_nominal()` are used to
#'  select columns based on their data type. Nominal variables include both
#'  character and factor.
#'
#' **In most cases**, the selectors `all_numeric_predictors()` and
#'  `all_nominal_predictors()`, which select on role and type, will be the right
#'  approach for users.
#'
#'  See [selections] for more details.
#'
#'  `current_info()` is an internal function.
#'
#'  All of these functions have have limited utility outside of column selection
#'  in step functions.
#'
#' @param match A single character string for the query. Exact
#'  matching is used (i.e. regular expressions won't work).
#'
#' @return
#'
#' Selector functions return an integer vector.
#'
#' `current_info()` returns an environment with objects `vars` and `data`.
#'
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' rec <- recipe(biomass) %>%
#'   update_role(
#'     carbon, hydrogen, oxygen, nitrogen, sulfur,
#'     new_role = "predictor"
#'   ) %>%
#'   update_role(HHV, new_role = "outcome") %>%
#'   update_role(sample, new_role = "id variable") %>%
#'   update_role(dataset, new_role = "splitting indicator")
#'
#' recipe_info <- summary(rec)
#' recipe_info
#'
#' # Centering on all predictors except carbon
#' rec %>%
#'   step_center(all_predictors(), -carbon) %>%
#'   prep(training = biomass) %>%
#'   bake(new_data = NULL)
#'
#' @export
has_role <- function(match = "predictor") {
  roles <- peek_roles()
  # roles is potentially a list columns so we unlist `.x` below.
  lgl_matches <- purrr::map_lgl(roles, ~any(unlist(.x) %in% match))
  which(lgl_matches)
}

#' @export
#' @rdname has_role
all_predictors <- function() {
  has_role("predictor")
}

#' @export
#' @rdname has_role
all_numeric_predictors <- function() {
  intersect(has_role("predictor"), has_type("numeric"))
}

#' @export
#' @rdname has_role
all_nominal_predictors <- function() {
  intersect(has_role("predictor"), has_type("nominal"))
}


#' @export
#' @rdname has_role
all_outcomes <- function() {
  has_role("outcome")
}

#' @export
#' @rdname has_role
has_type <- function(match = "numeric") {
  types <- peek_types()
  lgl_matches <- purrr::map_lgl(types, ~any(.x %in% match))
  which(lgl_matches)
}

#' @export
#' @rdname has_role
all_numeric <- function() {
  has_type("numeric")
}

#' @export
#' @rdname has_role
all_nominal <- function() {
  has_type("nominal")
}

peek_roles <- function() {
  peek_info("role")
}

peek_types <- function() {
  peek_info("type")
}

peek_info <- function(col) {
  .data <- current_info()$data
  purrr::map(.data, ~.x[[col]])
}

## functions to get current variable info for selectors modeled after
## dplyr versions

#' @import rlang
cur_info_env <- env(empty_env())

local_current_info <- function(nested_info, frame = parent.frame()) {
  local_bindings(
    vars = nested_info$variable,
    data = nested_info$data,
    .env = cur_info_env,
    .frame = frame
  )
}

#' @export
#' @rdname has_role
current_info <- function() {
  cur_info_env %||% rlang::abort("Variable context not set")
}
