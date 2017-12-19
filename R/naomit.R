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
#'   prep(airquality, verbose = FALSE, retain = TRUE) %>%
#'   juice()
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_naomit <- function(recipe, ..., role = NA, trained = FALSE) {
  add_step(
    recipe,
    step_naomit_new(
      terms = check_ellipses(...),
      role = role,
      trained = trained
    )
  )
}

step_naomit_new <- function(terms = NULL, role = NA, trained = FALSE) {
  step(
    subclass = "naomit",
    terms = terms,
    role = role,
    trained = trained
  )
}

#' @export
prep.step_naomit <- function(x, training, info = NULL, ...) {
  x$col_names <- terms_select(x$terms, info = info)
  x$trained <- TRUE
  x
}

#' @export
bake.step_naomit <- function(object, newdata, ...) {
  tibble::as_tibble(tidyr::drop_na(newdata, object$col_names))
}

print.step_naomit <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Removing rows with NA values in ", sep = "")
    cat(format_selectors(x$terms, wdth = width))
    cat("\n")
    invisible(x)
  }

#' @rdname step_naomit
#' @param x A `step_naomit` object.
tidy.step_naomit <- function(x, ...) {
  simple_terms(x, ...)
}
