#' Impute via bagged trees
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_bag()] instead.
#'
#' @keywords internal
#' @export
step_bagimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  impute_with = imp_vars(all_predictors()),
  trees = 25,
  models = NULL,
  options = list(keepX = FALSE),
  seed_val = sample.int(10^4, 1),
  skip = FALSE,
  id = rand_id("impute_bag")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_bagimpute()",
    with = "recipes::step_impute_bag()"
  )
}
