#' Remove observations with missing values
#'
#' `step_naomit` creates a *specification* of a recipe step that
#'   will add remove observations (rows of data) if they contain NA
#'   or NaN values.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the dummy variables. See
#'  [selections()] for more details. The selected
#'  variables must be factors.
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = FALSE`; in most instances that
#'  affect the rows of the data being predicted, this step probably should not
#'  be applied.
#'
#' @rdname step_naomit
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#'
#' recipe(Ozone ~ ., data = airquality) %>%
#'   step_naomit(Solar.R) %>%
#'   prep(airquality, verbose = FALSE) %>%
#'   juice()
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_naomit <- function(recipe, ..., role = NA, trained = FALSE,
                        columns = NULL, skip = FALSE,
                        id = rand_id("naomit")) {
  add_step(
    recipe,
    step_naomit_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_naomit_new <- function(terms, role, trained, columns, skip, id) {
  step(
    subclass = "naomit",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_naomit <- function(x, training, info = NULL, ...) {
  step_naomit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = terms_select(x$terms, info = info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_naomit <- function(object, new_data, ...) {
  tibble::as_tibble(tidyr::drop_na(new_data, object$columns))
}

print.step_naomit <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Removing rows with NA values in ", sep = "")
    cat(format_selectors(x$terms, width = width))
    cat("\n")
    invisible(x)
  }

#' @rdname step_naomit
#' @param x A `step_naomit` object.
#' @export
tidy.step_naomit <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}
