#' Impute numeric data using a rolling window statistic
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_roll()] instead.
#'
#' @keywords internal
#' @export
step_rollimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  statistic = median,
  window = 5,
  skip = FALSE,
  id = rand_id("impute_roll")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_rollimpute()",
    with = "recipes::step_impute_roll()"
  )
}
