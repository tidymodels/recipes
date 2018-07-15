#' Convert sentinel values to NA
#'
#' `step_sentinel` creates a *specification* of a recipe step that
#'   will convert specified sentinel values to `NA`.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param sentinels A list of sentinel values that indicate missingness and
#'   should be converted to explicit `NA` values. (Careful of type conversion)
#'   how to check for `NaN` and `Inf`??. Defaults to `BLAH`.
#' @param preset One of TODO. Defaults to `NULL`.
#'   - `"SAS"` specifies TODO
#'   - `"SPSS"` specifies TODO
#'   - etc etc
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#'
#' TODO
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()] [step_naomit()]
#'   [step_indicate()]
step_sentinel <-
  function(recipe,
           ...,
           role = TODO,
           trained = FALSE,
           sentinels = list(NaN, Inf),
           preset = NULL,
           skip = FALSE) {
    add_step(
      recipe,
      step_sentinel_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        sentinels = sentinels,
        preset = preset,
        skip = skip
      )
    )
  }

step_sentinel_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           sentinels = list(NaN, Inf),
           preset = NULL,
           skip = FALSE) {
    step(
      subclass = "sentinel",
      terms = terms,
      role = role,
      trained = trained,
      sentinels = sentinels,
      preset = preset,
      skip = skip)
  }

#' @export
prep.sentinel <- function(x, training, info = NULL, ...) {
  x$columns <- terms_select(x$terms, info = info)
  x$trained <- TRUE
  x
}

#' @export
bake.step_sentinel <- function(object, newdata, ...) {

  TODO

}

print.step_sentinel <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Converting sentinels to NA for ",  sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }
