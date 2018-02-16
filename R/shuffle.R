#' Shuffle Variables
#'
#' `step_shuffle` creates a *specification* of a recipe
#'  step that will randomly change the order of rows for selected
#'  variables.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will permuted. See [selections()] for more
#'  details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string that contains the names of
#'  columns that should be shuffled. These values are not determined
#'  until [prep.recipe()] is called.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @keywords datagen
#' @concept preprocessing randomization permutation
#' @export
#' @examples
#' integers <- data.frame(A = 1:12, B = 13:24, C = 25:36)
#'
#' library(dplyr)
#' rec <- recipe(~ A + B + C, data = integers) %>%
#'   step_shuffle(A, B)
#'
#' rand_set <- prep(rec, training = integers)
#'
#' set.seed(5377)
#' bake(rand_set, integers)
#'
#' tidy(rec, number = 1)
#' tidy(rand_set, number = 1)

step_shuffle <- function(recipe,
                         ...,
                         role = NA,
                         trained = FALSE,
                         columns = NULL,
                         skip = FALSE) {
  add_step(recipe,
           step_shuffle_new(
             terms = ellipse_check(...),
             role = role,
             trained = trained,
             columns = columns,
             skip = skip
           ))
}

step_shuffle_new <- function(terms = NULL,
                             role = NA,
                             trained = FALSE,
                             columns = NULL,
                             skip = FALSE) {
  step(
    subclass = "shuffle",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    skip = skip
  )
}

#' @export
prep.step_shuffle <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_shuffle_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip
  )
}

#' @export
bake.step_shuffle <- function(object, newdata, ...) {
  if (nrow(newdata) == 1) {
    warning("`newdata` contains a single row; unable to shuffle",
            call. = FALSE)
    return(newdata)
  }

  if (length(object$columns) > 0)
    for (i in seq_along(object$columns))
      newdata[, object$columns[i]] <-
        sample(getElement(newdata, object$columns[i]))
    as_tibble(newdata)
}

print.step_shuffle <-
  function(x, width = max(20, options()$width - 22), ...) {
    cat("Shuffled ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_shuffle
#' @param x A `step_shuffle` object.
tidy.step_shuffle <- function(x, ...) {
  simple_terms(x, ...)
}
