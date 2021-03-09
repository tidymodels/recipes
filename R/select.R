#' Select variables using dplyr
#'
#' `step_select()` creates a *specification* of a recipe step
#'  that will select variables using [dplyr::select()].
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables will be selected when baking. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms selected by this step, what analysis
#'  role should they be assigned?
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with column `terms` which
#'  contains the `select` expressions as character strings
#'  (and are not reparsable).
#' @details When an object in the user's global environment is
#'  referenced in the expression defining the new variable(s),
#'  it is a good idea to use quasiquotation (e.g. `!!`) to embed
#'  the value of the object in the expression (to be portable
#'  between sessions). See the examples.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_filters
#' @export
#' @examples
#' library(dplyr)
#'
#' iris_tbl <- as_tibble(iris)
#' iris_train <- slice(iris_tbl, 1:75)
#' iris_test <- slice(iris_tbl, 76:150)
#'
#' dplyr_train <- select(iris_train, Species, starts_with("Sepal"))
#' dplyr_test <- select(iris_test, Species, starts_with("Sepal"))
#'
#' rec <- recipe(~., data = iris_train) %>%
#'   step_select(Species, starts_with("Sepal")) %>%
#'   prep(training = iris_train)
#'
#' rec_train <- bake(rec, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' rec_test <- bake(rec, iris_test)
#' all.equal(dplyr_test, rec_test)
#'
#' # Local variables
#' sepal_vars <- c("Sepal.Width", "Sepal.Length")
#'
#' qq_rec <-
#'   recipe(~., data = iris_train) %>%
#'   # fine for interactive usage
#'   step_select(Species, all_of(sepal_vars)) %>%
#'   # best approach for saving a recipe to disk
#'   step_select(Species, all_of(!!sepal_vars))
#'
#' # Note that `sepal_vars` is inlined in the second approach
#' qq_rec
step_select <- function(recipe,
                        ...,
                        role = NA,
                        trained = FALSE,
                        skip = FALSE,
                        id = rand_id("select")) {
  add_step(
    recipe,
    step_select_new(
      terms = ellipse_check(...),
      trained = trained,
      role = role,
      skip = skip,
      id = id
    )
  )
}
step_select_new <- function(terms, role, trained, skip, id) {
    step(
      subclass = "select",
      terms = terms,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_select <- function(x, training, info = NULL, ...) {
  terms <- eval_select_recipes(x$terms, training, info, allow_rename = TRUE)

  step_select_new(
    terms = terms,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select <- function(object, new_data, ...) {
  dplyr::select(new_data, dplyr::all_of(object$terms))
}


print.step_select <-
  function(x, width = max(20, options()$width - 35), ...) {
    if (x$trained) {
      cat(
        "Variables selected ",
        paste0(names(x$terms), collapse = ", ")
      )
    } else {
      cat(
        "Terms selected ",
        paste0(x$terms, collapse = ", ")
      )
    }
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }

#' @rdname step_select
#' @param x A `step_select` object
#' @export
tidy.step_select <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$terms))
  } else {
    var_expr <- map(x$terms, quo_get_expr)
    var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
    res <- tibble(terms = var_expr)
  }
  res$id <- x$id
  res
}
