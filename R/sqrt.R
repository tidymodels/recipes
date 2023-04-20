#' Square Root Transformation
#'
#' `step_sqrt` creates a *specification* of a recipe
#'  step that will square root transform the data.
#'
#' @inheritParams step_center
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @template step-return
#' @family individual transformation steps
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#' `terms` (the columns that will be affected) is returned.
#'
#' @template case-weights-not-supported
#'
#' @export
#' @examples
#' set.seed(313)
#' examples <- matrix(rnorm(40)^2, ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' sqrt_trans <- rec %>%
#'   step_sqrt(all_numeric_predictors())
#'
#' sqrt_obj <- prep(sqrt_trans, training = examples)
#'
#' transformed_te <- bake(sqrt_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(sqrt_trans, number = 1)
#' tidy(sqrt_obj, number = 1)
step_sqrt <- function(recipe, ..., role = NA,
                      trained = FALSE, columns = NULL,
                      skip = FALSE,
                      id = rand_id("sqrt")) {
  add_step(
    recipe,
    step_sqrt_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_sqrt_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "sqrt",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_sqrt <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  step_sqrt_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sqrt <- function(object, new_data, ...) {
  check_new_data(names(object$columns), object, new_data)

  col_names <- object$columns
  for (i in seq_along(col_names)) {
    new_data[[col_names[i]]] <- sqrt(new_data[[col_names[i]]])
  }
  new_data
}

print.step_sqrt <- function(x, width = max(20, options()$width - 29), ...) {
  title <- "Square root transformation on "
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_sqrt <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
