#' Sort rows using dplyr
#'
#' `step_arrange()` creates a *specification* of a recipe step that will sort
#' rows using [dplyr::arrange()].
#'
#' @inheritParams step_center
#' @param ... Comma separated list of unquoted variable names. Use `desc()`` to
#'   sort a variable in descending order. See [dplyr::arrange()] for more
#'   details.
#' @param inputs Quosure of values given by `...`.
#' @template step-return
#' @details
#'
#' When an object in the user's global environment is referenced in the
#' expression defining the new variable(s), it is a good idea to use
#' quasiquotation (e.g. `!!!`) to embed the value of the object in the
#' expression (to be portable between sessions). See the examples.
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
#' @family dplyr steps
#' @export
#' @examples
#' rec <- recipe(~., data = iris) |>
#'   step_arrange(desc(Sepal.Length), 1 / Petal.Length)
#'
#' prepped <- prep(rec, training = iris |> slice(1:75))
#' tidy(prepped, number = 1)
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris |>
#'   as_tibble() |>
#'   slice(1:75) |>
#'   dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris |>
#'   as_tibble() |>
#'   slice(76:150) |>
#'   dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)
#' rec_test <- bake(prepped, iris |> slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # When you have variables/expressions, you can create a
#' # list of symbols with `rlang::syms()`` and splice them in
#' # the call with `!!!`. See https://tidyeval.tidyverse.org
#'
#' sort_vars <- c("Sepal.Length", "Petal.Length")
#'
#' qq_rec <-
#'   recipe(~., data = iris) |>
#'   # Embed the `values` object in the call using !!!
#'   step_arrange(!!!syms(sort_vars)) |>
#'   prep(training = iris)
#'
#' tidy(qq_rec, number = 1)
step_arrange <- function(
  recipe,
  ...,
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

#' @export
print.step_arrange <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Row arrangement using "
    print_step(x$inputs, x$inputs, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_arrange <- function(x, ...) {
  cond_expr <- unname(x$inputs)
  cond_expr <- map(cond_expr, quo_get_expr)
  cond_expr <- map_chr(cond_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = cond_expr,
    id = rep(x$id, length(x$inputs))
  )
}

#' @export
.recipes_preserve_sparsity.step_arrange <- function(x, ...) {
  TRUE
}
