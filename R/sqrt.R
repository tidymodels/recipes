#' Square Root Transformation
#'
#' `step_sqrt` creates a *specification* of a recipe
#'  step that will square root transform the data.

#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples
#' set.seed(313)
#' examples <- matrix(rnorm(40)^2, ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' sqrt_trans <- rec  %>%
#'   step_sqrt(all_predictors())
#'
#' sqrt_obj <- prep(sqrt_trans, training = examples)
#'
#' transformed_te <- bake(sqrt_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(sqrt_trans, number = 1)
#' tidy(sqrt_obj, number = 1)
#' @seealso [step_logit()] [step_invlogit()]
#'   [step_log()]  [step_hyperbolic()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]

step_sqrt <- function(recipe, ..., role = NA,
                      trained = FALSE, columns = NULL,
                      skip = FALSE) {
  add_step(
    recipe,
    step_sqrt_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip
    )
  )
}

step_sqrt_new <-
  function(terms = NULL, role = NA, trained = FALSE,
           columns = NULL,
           skip = FALSE) {
    step(
      subclass = "sqrt",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip
    )
  }


#' @export
prep.step_sqrt <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  step_sqrt_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip
  )
}

#' @export
bake.step_sqrt <- function(object, newdata, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <-
      sqrt(getElement(newdata, col_names[i]))
  as_tibble(newdata)
}

print.step_sqrt <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Square root transformation on ", sep = "")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_sqrt
#' @param x A `step_sqrt` object.
tidy.step_sqrt <- function(x, ...) {
  simple_terms(x, ...)
}
