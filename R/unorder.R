#' Convert ordered factors to unordered factors
#'
#' `step_unorder()` creates a *specification* of a recipe step that will turn
#' ordered factor variables into unordered factor variables.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' The factors level order is preserved during the transformation.
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
#' lmh <- c("Low", "Med", "High")
#'
#' examples <- data.frame(
#'   X1 = factor(rep(letters[1:4], each = 3)),
#'   X2 = ordered(rep(lmh, each = 4),
#'     levels = lmh
#'   )
#' )
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' factor_trans <- rec |>
#'   step_unorder(all_nominal_predictors())
#'
#' factor_obj <- prep(factor_trans, training = examples)
#'
#' transformed_te <- bake(factor_obj, examples)
#' table(transformed_te$X2, examples$X2)
#'
#' tidy(factor_trans, number = 1)
#' tidy(factor_obj, number = 1)
step_unorder <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("unorder")
  ) {
    add_step(
      recipe,
      step_unorder_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_unorder_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "unorder",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_unorder <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  step_unorder_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_unorder <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <-
      factor(
        x = as.character(new_data[[col_name]]),
        levels = levels(new_data[[col_name]])
      )
  }
  new_data
}

#' @export
print.step_unorder <-
  function(x, width = max(20, options()$width - 33), ...) {
    title <- "Unordered variables "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_unorder <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
