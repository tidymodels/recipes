#' Impute via k-nearest neighbors
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_impute_knn()] instead.
#'
#' @keywords internal
#' @export
step_knnimpute <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  neighbors = 5,
  impute_with = imp_vars(all_predictors()),
  options = list(nthread = 1, eps = 1e-08),
  ref_data = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("impute_knn")
) {
  lifecycle::deprecate_stop(
    when = "0.1.16",
    what = "recipes::step_knnimpute()",
    with = "recipes::step_impute_knn()"
  )
}
