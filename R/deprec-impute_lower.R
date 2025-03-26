#' Impute numeric data below the threshold of measurement
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_lower()] instead.
#'
#' @keywords internal
#' @export
step_lowerimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = NULL,
  skip = FALSE,
  id = rand_id("impute_lower")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_lowerimpute()",
    with = "recipes::step_impute_lower()"
  )
}
