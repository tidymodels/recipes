#' Logit Transformation
#'
#' `step_logit` creates a *specification* of a recipe
#'  step that will logit transform the data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
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
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @details The logit transformation takes values between
#'  zero and one and translates them to be on the real line using
#'  the function `f(p) = log(p/(1-p))`.
#' @examples
#' set.seed(313)
#' examples <- matrix(runif(40), ncol = 2)
#' examples <- data.frame(examples)
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' logit_trans <- rec  %>%
#'   step_logit(all_predictors())
#'
#' logit_obj <- prep(logit_trans, training = examples)
#'
#' transformed_te <- bake(logit_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#'
#' tidy(logit_trans, number = 1)
#' tidy(logit_obj, number = 1)
#' @seealso [step_invlogit()] [step_log()]
#' [step_sqrt()]  [step_hyperbolic()] [recipe()]
#' [prep.recipe()] [bake.recipe()]

step_logit <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("logit")) {
    add_step(recipe,
             step_logit_new(
               terms = ellipse_check(...),
               role = role,
               trained = trained,
               columns = columns,
               skip = skip,
               id = id
             ))
  }

step_logit_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "logit",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_logit <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_type(training[, col_names])

  step_logit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_logit <- function(object, new_data, ...) {
  for (i in seq_along(object$columns))
    new_data[, object$columns[i]] <-
      binomial()$linkfun(getElement(new_data, object$columns[i]))
  as_tibble(new_data)
}


print.step_logit <-
  function(x, width = max(20, options()$width - 33), ...) {
    cat("Logit transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_logit
#' @param x A `step_logit` object.
#' @export
tidy.step_logit <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
