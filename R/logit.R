#' Logit Transformation
#'
#' `step_logit` creates a *specification* of a recipe
#'  step that will logit transform the data.
#'
#' @inheritParams step_center
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param offset A numeric value to modify values of the columns that are either
#' one or zero. They are modified to be `x - offset` or `offset`, respectively.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details The logit transformation takes values between
#'  zero and one and translates them to be on the real line using
#'  the function `f(p) = log(p/(1-p))`.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the columns that will be affected) is returned.
#'
#' @template case-weights-not-supported
#'
#' @examples
#' set.seed(313)
#' examples <- matrix(runif(40), ncol = 2)
#' examples <- data.frame(examples)
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' logit_trans <- rec %>%
#'   step_logit(all_numeric_predictors())
#'
#' logit_obj <- prep(logit_trans, training = examples)
#'
#' transformed_te <- bake(logit_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#'
#' tidy(logit_trans, number = 1)
#' tidy(logit_obj, number = 1)
step_logit <-
  function(recipe,
           ...,
           offset = 0,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("logit")) {
    add_step(
      recipe,
      step_logit_new(
        terms = enquos(...),
        offset = offset,
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_logit_new <-
  function(terms, offset, role, trained, columns, skip, id) {
    step(
      subclass = "logit",
      terms = terms,
      offset = offset,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_logit <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  step_logit_new(
    terms = x$terms,
    offset = x$offset,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

pre_logit <- function(x, eps = 0) {
  x <- ifelse(x == 1, x - eps, x)
  x <- ifelse(x == 0, eps, x)
  x
}

#' @export
bake.step_logit <- function(object, new_data, ...) {
  check_new_data(names(object$columns), object, new_data)

  for (i in seq_along(object$columns)) {
    new_data[[object$columns[i]]] <-
      binomial()$linkfun(
        pre_logit(new_data[[object$columns[i]]], object$offset)
      )
  }
  new_data
}


print.step_logit <-
  function(x, width = max(20, options()$width - 33), ...) {
    title <- "Logit transformation on "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_logit <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
