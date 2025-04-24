#' Shuffle variables
#'
#' `step_shuffle()` creates a *specification* of a recipe step that will
#' randomly change the order of rows for selected variables.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @template step-return
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
#' @template sparse-preserve
#'
#' @template case-weights-not-supported
#'
#' @family row operation steps
#' @export
#' @examples
#' integers <- data.frame(A = 1:12, B = 13:24, C = 25:36)
#'
#' library(dplyr)
#' rec <- recipe(~ A + B + C, data = integers) |>
#'   step_shuffle(A, B)
#'
#' rand_set <- prep(rec, training = integers)
#'
#' set.seed(5377)
#' bake(rand_set, integers)
#'
#' tidy(rec, number = 1)
#' tidy(rand_set, number = 1)
step_shuffle <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("shuffle")
) {
  add_step(
    recipe,
    step_shuffle_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_shuffle_new <- function(terms, role, trained, columns, skip, id) {
  step(
    subclass = "shuffle",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_shuffle <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  step_shuffle_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_shuffle <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  if (nrow(new_data) == 1) {
    cli::cli_warn("{.arg new_data} contains a single row; unable to shuffle.")
    return(new_data)
  }

  for (col_name in col_names) {
    new_data[[col_name]] <- sample(new_data[[col_name]])
  }

  new_data
}

#' @export
print.step_shuffle <-
  function(x, width = max(20, options()$width - 22), ...) {
    title <- "Shuffled "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_shuffle <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_shuffle <- function(x, ...) {
  TRUE
}
