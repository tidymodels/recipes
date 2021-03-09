#' Select variables using `select`
#'
#' `step_select` creates a *specification* of a recipe step
#'  that will select variables using [dplyr::select()].
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables will be selected when baking. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms selected by this step, what analysis
#'  role should they be assigned?
#' @param inputs Quosure(s) of `...`.
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
#' rec <-
#'   recipe(~., data = iris) %>%
#'   step_select(Species, starts_with("Sepal"))
#'
#' prepped <- prep(rec, training = iris %>% slice(1:75))
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(1:75) %>%
#'   select(Species, starts_with("Sepal"))
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(76:150) %>%
#'   select(Species, starts_with("Sepal"))
#' rec_test <- bake(prepped, iris %>% slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # Embedding objects:
#' sepal_vars <- c("Sepal.Width", "Sepal.Length")
#'
#' qq_rec <-
#'   recipe(~., data = iris) %>%
#'   # bad approach
#'   step_select(Species, sepal_vars) %>%
#'   # best approach
#'   step_select(Species, !!sepal_vars) %>%
#'   prep(training = iris)
#'
#' bake(qq_rec, new_data = NULL) %>% slice(1:4)
#'
#' # The difference:
#' tidy(qq_rec, number = 1)
#' tidy(qq_rec, number = 2)
step_select <- function(recipe,
                        ...,
                        role = NA,
                        trained = FALSE,
                        inputs = NULL,
                        skip = FALSE,
                        id = rand_id("select")) {
  add_step(
    recipe,
    step_select_new(
      terms = ellipse_check(...),
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}
step_select_new <- function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "select",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_select <- function(x, training, info = NULL, ...) {
  sel <- eval_select_recipes(x$terms, training, info, allow_rename = TRUE)

  step_select_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = sel,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select <- function(object, new_data, ...) {
  dplyr::select(new_data, object$inputs)
}


print.step_select <-
  function(x, width = max(20, options()$width - 35), ...) {
    if (x$trained) {
      cat(
        "Variables selected ",
        paste0(names(x$inputs), collapse = ", ")
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
    res <- tibble(terms = x$inputs)
  } else {
    var_expr <- map(x$terms, quo_get_expr)
    var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
    res <- tibble(terms = var_expr)
  }
  res$id <- x$id
  res
}
