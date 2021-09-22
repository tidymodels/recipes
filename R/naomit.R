#' Remove observations with missing values
#'
#' `step_naomit` creates a *specification* of a recipe step that
#'   will remove observations (rows of data) if they contain `NA`
#'   or `NaN` values.
#'
#' @template row-ops
#' @inheritParams step_center
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#'
#' @template step-return
#' @family row operation steps
#' @export
#'
#' @examples
#'
#' recipe(Ozone ~ ., data = airquality) %>%
#'   step_naomit(Solar.R) %>%
#'   prep(airquality, verbose = FALSE) %>%
#'   bake(new_data = NULL)
#'
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
    columns = recipes_eval_select(x$terms, training, info),
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

#' @rdname tidy.recipe
#' @export
tidy.step_naomit <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}
