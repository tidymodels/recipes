#' Estimate a preprocessing recipe
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [prep()] instead.
#'
#' @keywords internal
#' @export
prepare <- function(x, ...) {
  lifecycle::deprecate_stop("0.0.1.9006", "prepare()", "prep()")
}
