#' Add new variables using `mutate`
#'
#' `step_mutate` creates a *specification* of a recipe step
#'  that will add variables using [dplyr::mutate()].
#'
#' @inheritParams step_center
#' @param ... Name-value pairs of expressions. See [dplyr::mutate()].
#' If the argument is not named, the expression is converted to
#' a column name.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? By default, the function assumes
#'  that the new dimension columns created by the original variables
#'  will be used as predictors in a model.
#' @param inputs Quosure(s) of `...`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `values` which
#'  contains the `mutate` expressions as character strings
#'  (and are not reparsable).
#' @details When an object in the user's global environment is
#'  referenced in the expression defining the new variable(s),
#'  it is a good idea to use quasiquotation (e.g. `!!`) to embed
#'  the value of the object in the expression (to be portable
#'  between sessions). See the examples.
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @examples
#' rec <-
#'   recipe( ~ ., data = iris) %>%
#'   step_mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#'
#' prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(1:75) %>%
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#'
#' rec_train <- juice(prepped)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(76:150) %>%
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#' rec_test <- bake(prepped, iris %>% slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # Embedding objects:
#' const <- 1.414
#'
#' qq_rec <-
#'   recipe( ~ ., data = iris) %>%
#'   step_mutate(
#'     bad_approach = Sepal.Width * const,
#'     best_approach = Sepal.Width * !!const
#'   ) %>%
#'   prep(training = iris, retain = TRUE)
#'
#' juice(qq_rec, contains("appro")) %>% slice(1:4)
#'
#' # The difference:
#' tidy(qq_rec, number = 1)

step_mutate <- function(
  recipe, ...,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate")
) {

  inputs <- enquos(..., .named = TRUE)

  add_step(
    recipe,
    step_mutate_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_mutate_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "mutate",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_mutate <- function(x, training, info = NULL, ...) {
  step_mutate_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_mutate <- function(object, new_data, ...) {
  dplyr::mutate(new_data, !!!object$inputs)
}


print.step_mutate <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Variable mutation for ",
        paste0(names(x$inputs), collapse = ", "))
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }

#' @rdname step_mutate
#' @param x A `step_mutate` object
#' @export
tidy.step_mutate <- function(x, ...) {
  var_expr <- map(x$inputs, quo_get_expr)
  var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
    tibble(
      terms = names(x$inputs),
      value = var_expr,
      id = rep(x$id, length(x$inputs))
    )
}


# ------------------------------------------------------------------------------

#' Mutate multiple columns
#'
#' `step_mutate_at` creates a *specification* of a recipe step that will modify
#' the selected variables using a common function.
#'
#' @inheritParams step_center
#' @param fn A more limited function argument than [dplyr::mutate_at()]. `fn`
#' should be a single function and cannot be a a quosure style lambda. Currently,
#' extra arguments cannot be passed to `fn`.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the function assumes that the new dimension
#'  columns created by the original variables will be used as predictors in a
#'  model.
#' @param inputs A vector of column names populated by `prep()`.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` which contains the columns being transformed.
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @examples
#' library(dplyr)
#' recipe(~ ., data = iris) %>%
#'   step_mutate_at(contains("Length"), fn = function(x) -x) %>%
#'   prep() %>%
#'   juice() %>%
#'   slice(1:10)
#' @export
step_mutate_at <- function(
  recipe, ...,
  fn,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate_at")
) {

  add_step(
    recipe,
    step_mutate_at_new(
      terms = ellipse_check(...),
      fn = fn,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_mutate_at_new <-
  function(terms, fn, role, trained, inputs, skip, id) {
    step(
      subclass = "mutate_at",
      terms = terms,
      fn = fn,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_mutate_at <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_mutate_at_new(
    terms = x$terms,
    fn = x$fn,
    trained = TRUE,
    role = x$role,
    inputs = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_mutate_at <- function(object, new_data, ...) {
  dplyr::mutate_at(new_data, .vars = object$inputs, .funs = object$fn)
}


print.step_mutate_at <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Variable mutation for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_mutate
#' @export
tidy.step_mutate_at <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$inputs)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

