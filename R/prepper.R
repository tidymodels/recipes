#' Wrapper function for preparing recipes within resampling
#'
#' When working with the \pkg{rsample} package, a simple recipe must be
#' *prepared* using the `prep` function first. When using recipes with
#' \pkg{rsample} it is helpful to have a function that can prepare a recipe
#' across a series of `split` objects that are produced in this package.
#' `prepper` is a wrapper function around [prep()] that can be used to do this.
#' See the vignette on "Recipes and rsample" for an example.
#'
#' @details
#'
#' `prepper()` sets the underlying [prep()] argument `fresh` to `TRUE`.
#'
#' @param split_obj An `rplit` object
#' @param recipe An untrained `recipe` object.
#' @param ... Arguments to pass to `prep` such as `verbose` or `retain`.
#' @export
prepper <- function(split_obj, recipe, ...) {
  prep(recipe, training = rsample::analysis(split_obj), fresh = TRUE, ...)
}
# alternate names: chef? dr_prepper?
