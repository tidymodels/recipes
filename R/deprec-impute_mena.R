#' Impute numeric data using the mean
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_mean()] instead.
#'
#' @keywords internal
#' @export
step_meanimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  means = NULL,
  trim = 0,
  skip = FALSE,
  id = rand_id("impute_mean")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_meanimpute()",
    with = "recipes::step_impute_mean()"
  )
}
