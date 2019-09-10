#' Sort rows using dplyr
#'
#' `step_arrange` creates a *specification* of a recipe step
#'  that will sort rows using [dplyr::arrange()].
#'
#' @inheritParams step_center
#' @param ... Comma separated list of unquoted variable names.
#'  Use `desc()`` to sort a variable in descending order. See
#'  [dplyr::arrange()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param inputs Quosure of values given by `...`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  contains the sorting variable(s) or expression(s). The
#'  expressions are text representations and are not parsable.
#' @details When an object in the user's global environment is
#'  referenced in the expression defining the new variable(s),
#'  it is a good idea to use quasiquotation (e.g. `!!!`)
#'   to embed the value of the object in the expression (to
#'   be portable between sessions). See the examples.
#' @keywords datagen
#' @concept preprocessing
#' @export
#' @examples
#' rec <- recipe( ~ ., data = iris) %>%
#'   step_arrange(desc(Sepal.Length), 1/Petal.Length)
#'
#' prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)
#' tidy(prepped, number = 1)
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(1:75) %>%
#'   dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)
#'
#' rec_train <- juice(prepped)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(76:150) %>%
#'   dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)
#' rec_test <- bake(prepped, iris %>% slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # When you have variables/expressions, you can create a
#' # list of symbols with `rlang::syms()`` and splice them in
#' # the call with `!!!`. See https://tidyeval.tidyverse.org
#'
#' sort_vars <- c("Sepal.Length", "Petal.Length")
#'
#' qq_rec <-
#'   recipe( ~ ., data = iris) %>%
#'   # Embed the `values` object in the call using !!!
#'   step_arrange(!!!syms(sort_vars)) %>%
#'   prep(training = iris)
#'
#' tidy(qq_rec, number = 1)

step_arrange <- function(
  recipe, ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("arrange")
) {

  inputs <- enquos(...)

  add_step(
    recipe,
    step_arrange_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_arrange_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "arrange",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_arrange <- function(x, training, info = NULL, ...) {
  step_arrange_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_arrange <- function(object, new_data, ...) {
  dplyr::arrange(new_data, !!!object$inputs)
}


print.step_arrange <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Row arrangement")
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }

#' @rdname step_arrange
#' @param x A `step_arrange` object
#' @export
tidy.step_arrange <- function(x, ...) {
  cond_expr <- map(x$inputs, quo_get_expr)
  cond_expr <- map_chr(cond_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = cond_expr,
    id = rep(x$id, length(x$inputs))
  )
}
