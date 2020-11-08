

#' @name selections
#' @aliases selections
#' @aliases selection
#'
#' @title Methods for Selecting Variables in Step Functions
#'
#' @description When selecting variables or model terms in `step`
#'  functions, `dplyr`-like tools are used. The *selector* functions
#'  can choose variables based on their name, current role, data
#'  type, or any combination of these. The selectors are passed as
#'  any other argument to the step. If the variables are explicitly
#'  stated in the step function, this might be similar to:
#'
#' \preformatted{
#'   recipe( ~ ., data = USArrests) \%>\%
#'     step_pca(Murder, Assault, UrbanPop, Rape, num_comp = 3)
#' }
#'
#'  The first four arguments indicate which variables should be
#'  used in the PCA while the last argument is a specific argument
#'  to [step_pca()].
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
#'   has the effect of adding all variables to the list except the
#'   excluded variable(s).
#'   }
#'
#' Also, select helpers from the `tidyselect` package can also be used:
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
#' There are sets of recipes specific functions that can be used to select
#' variables based on their role or type: [has_role()] and
#' [has_type()]. For convenience, there are also functions that are
#' more specific: [all_numeric()], [all_nominal()],
#' [all_predictors()], and [all_outcomes()]. These can be used in
#' conjunction with the previous functions described for selecting
#' variables using their names:
#'
#' \preformatted{
#'   data(biomass)
#'   recipe(HHV ~ ., data = biomass) \%>\%
#'     step_center(all_numeric(), -all_outcomes())
#' }
#'
#' This results in all the numeric predictors: carbon, hydrogen,
#' oxygen, nitrogen, and sulfur.
#'
#' If a role for a variable has not been defined, it will never be
#' selected using role-specific selectors.
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
NULL


eval_select_recipes <- function(quos, data, info) {
  # Maintain ordering between `data` column names and `info$variable` so
  # `eval_select()` and recipes selectors return compatible positions
  data_info <- tibble(variable = names(data))
  data_info <- dplyr::left_join(data_info, info, by = "variable")

  nested_info <- nest_current_info(data_info)

  local_current_info(nested_info)

  expr <- expr(c(!!!quos))

  # FIXME: Ideally this is `FALSE`, but empty selections incorrectly throw an
  # error when this is false due to the following bug:
  # https://github.com/r-lib/tidyselect/issues/221
  allow_rename <- TRUE

  sel <- tidyselect::eval_select(
    expr = expr,
    data = data,
    allow_rename = allow_rename
  )

  # Return names not positions, as these names are
  # used for both the training and test set and their positions
  # may have changed. `sel` won't be named because when `allow_rename = FALSE`,
  # `eval_select()` returns an unnamed vector.
  out <- names(data)[sel]

  # FIXME: Remove this check when the following issue is fixed,
  # i.e. when we can use `allow_rename = FALSE`
  # https://github.com/r-lib/tidyselect/issues/221
  if (!identical(out, names(sel))) {
    abort("Can't rename variables in this context.")
  }

  out
}

nest_current_info <- function(info) {
  # See https://tidyr.tidyverse.org/dev/articles/in-packages.html
  if (tidyr_new_interface()) {
    tidyr::nest(info, data = -variable)
  } else {
    tidyr::nest(info, -variable)
  }
}

#' Role Selection
#'
#' @description
#'
#' `has_role()`, `all_predictors()`, and `all_outcomes()` can be used to
#'  select variables in a formula that have certain roles.
#'  Similarly, `has_type()`, `all_numeric()`, and `all_nominal()` are used
#'  to select columns based on their data type.
#'
#'  See `?selections` for more details.
#'
#'  `current_info()` is an internal function.
#'
#'  All of these functions have have limited utility
#'  outside of column selection in step functions.
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
#' @keywords datagen
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
  lgl_matches <- purrr::map_lgl(roles, ~any(.x %in% match))
  which(lgl_matches)
}

#' @export
#' @rdname has_role
all_predictors <- function() {
  has_role("predictor")
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

# ------------------------------------------------------------------------------
# Old method for selection. This has been completely superseded by
# `eval_select_recipes()`, and should no longer be used in recipes, but we
# have exported it so we continue to support it here.

# This flags formulas that are not allowed
element_check <- function(x) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]

  # i.e. tidyselect::matches()
  funs <- funs[!(funs %in% c("::", "tidyselect", "dplyr", "recipes"))]

  name_selectors <- c(
    "starts_with",
    "ends_with",
    "contains",
    "matches",
    "num_range",
    "everything",
    "one_of",
    "all_of",
    "any_of",
    "c"
  )
  role_selectors <- c(
    "has_role",
    "all_predictors",
    "all_outcomes"
  )
  type_selectors <- c(
    "has_type",
    "all_numeric",
    "all_nominal"
  )
  selectors <- c(
    name_selectors,
    role_selectors,
    type_selectors
  )

  not_good <- funs[!(funs %in% selectors)]

  if (length(not_good) > 0) {
    rlang::abort(paste0(
      "Not all functions are allowed in step function selectors (e.g. ",
      paste0("`", not_good, "`", collapse = ", "),
      "). See ?selections."
    ))
  }

  invisible(NULL)
}

#' Select Terms in a Step Function.
#'
#' This function bakes the step function selectors and might be
#'  useful when creating custom steps.
#'
#' @param info A tibble with columns `variable`, `type`, `role`,
#'  and `source` that represent the current state of the data. The
#'  function [summary.recipe()] can be used to get this information
#'  from a recipe.
#' @param terms A list of formulas whose right-hand side contains
#'  quoted expressions. See [rlang::quos()] for examples.
#' @param empty_fun A function to execute when no terms are selected by the
#'  step. The default function throws an error with a message.
#' @keywords datagen
#' @concept preprocessing
#' @return A character string of column names or an error of there
#'  are no selectors or if no variables are selected.
#' @seealso [recipe()] [summary.recipe()]
#'   [prep.recipe()]
#' @export
#' @examples
#' library(rlang)
#' library(modeldata)
#' data(okc)
#' rec <- recipe(~ ., data = okc)
#' info <- summary(rec)
#' terms_select(info = info, quos(all_predictors()))
terms_select <- function(terms, info, empty_fun = abort_selection) {
  # unique in case a variable has multiple roles
  vars <- unique(info$variable)

  if (is_empty(terms)) {
    rlang::abort("At least one selector should be used")
  }

  ## check arguments against whitelist
  lapply(terms, element_check)

  # Set current_info so available to helpers

  nested_info <- nest_current_info(info)

  local_current_info(nested_info)

  # `terms` might be a single call (like in step_interact()),
  # or it could be a list of quosures.
  # They have to be unquoted differently
  if (is.call(terms)) {
    sel <- with_handlers(
      tidyselect::vars_select(vars, !! terms),
      tidyselect_empty = empty_fun
    )
  } else {
    sel <- with_handlers(
      tidyselect::vars_select(vars, !!! terms),
      tidyselect_empty = empty_fun
    )
  }

  unname(sel)
}

abort_selection <- exiting(function(cnd) {
  abort("No variables or terms were selected.")
})
