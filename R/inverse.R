#' Inverse Transformation
#'
#' `step_inverse` creates a *specification* of a recipe
#'  step that will inverse transform the data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param offset An optional value to add to the data prior to
#'  logging (to avoid `1/0`).
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @template step-return
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @details When you [`tidy()`] this step, a tibble with columns `terms`
#' (the columns that will be affected) is returned.
#' @examples
#' set.seed(313)
#' examples <- matrix(runif(40), ncol = 2)
#' examples <- data.frame(examples)
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' inverse_trans <- rec  %>%
#'   step_inverse(all_numeric_predictors())
#'
#' inverse_obj <- prep(inverse_trans, training = examples)
#'
#' transformed_te <- bake(inverse_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#'
#' tidy(inverse_trans, number = 1)
#' tidy(inverse_obj, number = 1)
#' @seealso [step_log()]
#' [step_sqrt()]  [step_hyperbolic()] [recipe()]
#' [prep.recipe()] [bake.recipe()]

step_inverse <-
  function(recipe,
           ...,
           role = NA,
           offset = 0,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("inverse")) {
    add_step(recipe,
             step_inverse_new(
               terms = ellipse_check(...),
               role = role,
               offset = offset,
               trained = trained,
               columns = columns,
               skip = skip,
               id = id
             ))
  }

step_inverse_new <-
  function(terms, role, offset, trained, columns, skip, id) {
    step(
      subclass = "inverse",
      terms = terms,
      role = role,
      offset = offset,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_inverse <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_type(training[, col_names])

  step_inverse_new(
    terms = x$terms,
    role = x$role,
    offset = x$offset,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_inverse <- function(object, new_data, ...) {
  for (i in seq_along(object$columns))
    new_data[, object$columns[i]] <-
      1 / (new_data [[ object$columns[i] ]] + object$offset)
  as_tibble(new_data)
}


print.step_inverse <-
  function(x, width = max(20, options()$width - 33), ...) {
    cat("Inverse transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_inverse` object.
#' @export
tidy.step_inverse <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}
