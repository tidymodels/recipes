#' Select terms in a step function.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `terms_select()` has been deprecated in favor of [recipes_eval_select()].
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
#'   [prep()]
#' @export
#' @keywords internal
terms_select <- function(terms, info, empty_fun = function(x) x) {
  lifecycle::deprecate_stop("1.0.6", "terms_select()", "recipes_eval_select()")
}
