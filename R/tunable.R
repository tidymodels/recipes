#' Find recommended methods for generating parameter values
#'
#' [tunable()] determines which parameters in an object _can_ be tuned along
#' with information about the parameters.
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#'  They also define the recommended objects from the `dials` package that can
#'  be used to generate new parameter values and other characteristics.
#' @param x A recipe, recipe step, or recipe check object
#' @param ... Not currently used.
#' @return A tibble with a column for the parameter `name`, information on the
#'  _default_ method for generating a corresponding parameter object, the
#'  `source` of the parameter (e.g. "recipe", etc.), and the `component` within
#'  the source. For the `component` column, a little more specificity is given
#'  about the location of the parameter (e.g. "step_normalize" for recipes).
#'  The `component_id` column contains the unique step `id` field.
#' @details
#' If the object has no tunable parameters, a tibble with no rows is returned.
#'
#' The information about the default parameter object takes the form of a
#' named list with an element for the function call and an optional element for
#' the source of the function (e.g. the `dials` package).
#' @export
#' @keywords internal
#'
#' @examples
#' \donttest{
#' library(recipes)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_knnimpute(all_predictors()) %>%
#'   step_pca(all_predictors()) %>%
#'   tunable()
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_normalize(all_predictors()) %>%
#'   tunable()
#' }
#'
#' @name tunable.recipe
#' @export
tunable.recipe <- function(x, ...) {
  if (length(x$steps) == 0) {
    res <- no_param
  } else {
    res <- purrr::map_dfr(x$steps, tunable)
    if (nrow(res) > 0) {
      res <- res[!is.na(res$name),]
    }
  }
  res
}

#' @rdname tunable.recipe
#' @export
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

#' @rdname tunable.recipe
#' @export
tunable.check <- function(x, ...) {
  no_param
}
