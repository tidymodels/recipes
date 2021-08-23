#' Select Terms in a Step Function.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `terms_select()` has been deprecated in favor of [recipes_eval_select()].
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
#' @return A character string of column names or an error of there
#'  are no selectors or if no variables are selected.
#' @seealso [recipe()] [summary.recipe()]
#'   [prep.recipe()]
#' @export
#' @keywords internal
#' @examples
#' library(rlang)
#' library(modeldata)
#' data(okc)
#' rec <- recipe(~ ., data = okc)
#' info <- summary(rec)
#' terms_select(info = info, quos(all_predictors()))
terms_select <- function(terms, info, empty_fun = abort_selection) {
  lifecycle::deprecate_soft("0.1.17", "terms_select()", "recipes_eval_select()")

  # unique in case a variable has multiple roles
  vars <- unique(info$variable)

  if (is_empty(terms)) {
    rlang::abort("At least one selector should be used")
  }

  ## check arguments against whitelist
  lapply(terms, element_check)

  # Set current_info so available to helpers

  nested_info <- tidyr::nest(info, data = -variable)

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
    "all_numeric_predictors",
    "all_nominal_predictors",
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

abort_selection <- exiting(function(cnd) {
  abort("No variables or terms were selected.")
})
