#' Impute nominal data using the most common value
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_mode()] instead.
#'
#' @keywords internal
#' @export
step_modeimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  modes = NULL,
  ptype = NULL,
  skip = FALSE,
  id = rand_id("impute_mode")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_modeimpute()",
    with = "recipes::step_impute_mode()"
  )
}
