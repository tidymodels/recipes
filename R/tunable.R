#' tunable methods for recipes
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#'  They also define the recommended objects from the `dials` package that can
#'  be used to generate new parameter values and other characteristics.
#' @param x A recipe step object
#' @param ... Not used.
#' @return A tibble object.
#' @export
#' @keywords internal
tunable.step <- function(x, ...) {
  no_param
}

step_type <- function(.step) class(.step)[class(.step) != "step"][1]

no_param <-
  tibble::tibble(
    name = NA_character_,
    call_info = list(),
    source = NA_character_,
    component = NA_character_,
    component_id = NA_character_
  )

