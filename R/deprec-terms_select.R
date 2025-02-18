#' Select terms in a step function.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [recipes_eval_select()] instead.
#'
#' @keywords internal
#' @export
terms_select <- function(terms, info, empty_fun = function(x) x) {
  lifecycle::deprecate_stop("1.0.6", "terms_select()", "recipes_eval_select()")
}
