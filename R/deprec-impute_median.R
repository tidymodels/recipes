#' Impute numeric data using the median
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_median()] instead.
#'
#' @keywords internal
#' @export
step_medianimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  medians = NULL,
  skip = FALSE,
  id = rand_id("impute_median")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_medianimpute()",
    with = "recipes::step_impute_median()"
  )
}
