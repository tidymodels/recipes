#' Inverse transformation
#'
#' `step_inverse()` creates a *specification* of a recipe step that will inverse
#' transform the data.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param offset An optional value to add to the data prior to logging (to avoid
#'   `1/0`).
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
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
#' inverse_trans <- rec |>
#'   step_inverse(all_numeric_predictors())
#'
#' inverse_obj <- prep(inverse_trans, training = examples)
#'
#' transformed_te <- bake(inverse_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#'
#' tidy(inverse_trans, number = 1)
#' tidy(inverse_obj, number = 1)
step_inverse <-
  function(
    recipe,
    ...,
    role = NA,
    offset = 0,
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("inverse")
  ) {
    add_step(
      recipe,
      step_inverse_new(
        terms = enquos(...),
        role = role,
        offset = offset,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
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
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$offset, arg = "offset")

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
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- 1 / (new_data[[col_name]] + object$offset)
  }

  new_data
}

#' @export
print.step_inverse <-
  function(x, width = max(20, options()$width - 33), ...) {
    title <- "Inverse transformation on "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_inverse <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
