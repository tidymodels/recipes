#' Hyperbolic Transformations
#'
#' `step_hyperbolic` creates a *specification* of a
#'  recipe step that will transform data using a hyperbolic
#'  function.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param func A character value for the function. Valid values
#'  are "sin", "cos", or "tan".
#' @param inverse A logical: should the inverse function be used?
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected), `inverse`, and `func`.
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @examples
#' set.seed(313)
#' examples <- matrix(rnorm(40), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' cos_trans <- rec  %>%
#'   step_hyperbolic(all_numeric_predictors(),
#'                   func = "cos", inverse = FALSE)
#'
#' cos_obj <- prep(cos_trans, training = examples)
#'
#' transformed_te <- bake(cos_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(cos_trans, number = 1)
#' tidy(cos_obj, number = 1)
#' @seealso [step_logit()] [step_invlogit()]
#'   [step_log()]  [step_sqrt()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]

step_hyperbolic <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           func = "sin",
           inverse = TRUE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("hyperbolic")) {
    funcs <- c("sin", "cos", "tan")
    if (!(func %in% funcs))
      rlang::abort("`func` should be either `sin``, `cos`, or `tan`")
    add_step(
      recipe,
      step_hyperbolic_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        func = func,
        inverse = inverse,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_hyperbolic_new <-
  function(terms, role, trained, func, inverse, columns, skip, id) {
    step(
      subclass = "hyperbolic",
      terms = terms,
      role = role,
      trained = trained,
      func = func,
      inverse = inverse,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_hyperbolic <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)
  check_type(training[, col_names])

  step_hyperbolic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    func = x$func,
    inverse = x$inverse,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_hyperbolic <- function(object, new_data, ...) {
  func <- if (object$inverse)
    get(paste0("a", object$func))
  else
    get(object$func)
  col_names <- object$columns
  for (i in seq_along(col_names))
    new_data[, col_names[i]] <-
    func(getElement(new_data, col_names[i]))
  as_tibble(new_data)
}

print.step_hyperbolic <-
  function(x, width = max(20, options()$width - 32), ...) {
    ttl <- paste("Hyperbolic", x$func)
    if (x$inverse)
      ttl <- paste(ttl, "(inv)")
    cat(ttl, "transformation on ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_hyperbolic
#' @param x A `step_hyperbolic` object.
#' @export
tidy.step_hyperbolic <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$inverse <- x$inverse
  out$func <- x$func
  out$id <- x$id
  out
}
