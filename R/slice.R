#' Filter rows by position using dplyr
#'
#' `step_slice()` creates a *specification* of a recipe step that will filter
#' rows using [dplyr::slice()].
#'
#' @template row-ops
#' @inheritParams step_center
#' @param ... Integer row values. See [dplyr::slice()] for more details.
#' @param inputs Quosure of values given by `...`.
#' @template step-return
#' @details
#'
#' When an object in the user's global environment is referenced in the
#' expression defining the new variable(s), it is a good idea to use
#' quasiquotation (e.g. `!!`) to embed the value of the object in the expression
#' (to be portable between sessions). See the examples.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, containing the filtering indices}
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
#'   step_slice(1:3)
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
#'   slice(1:3)
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris |>
#'   as_tibble() |>
#'   slice(76:150)
#'
#' rec_test <- bake(prepped, iris |> slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # Embedding the integer expression (or vector) into the
#' # recipe:
#'
#' keep_rows <- 1:6
#'
#' qq_rec <-
#'   recipe(~., data = iris) |>
#'   # Embed `keep_rows` in the call using !!!
#'   step_slice(!!!keep_rows) |>
#'   prep(training = iris)
#'
#' tidy(qq_rec, number = 1)
step_slice <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = TRUE,
  id = rand_id("slice")
) {
  inputs <- enquos(...)

  add_step(
    recipe,
    step_slice_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_slice_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "slice",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_slice <- function(x, training, info = NULL, ...) {
  step_slice_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_slice <- function(object, new_data, ...) {
  dplyr::slice(new_data, !!!object$inputs)
}

#' @export
print.step_slice <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Row filtering via position "
    tr_obj <- format_selectors(x$inputs, width)
    print_step(tr_obj, x$inputs, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_slice <- function(x, ...) {
  cond_expr <- map(x$inputs, quo_get_expr)
  cond_expr <- map_chr(cond_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = unname(cond_expr),
    id = rep(x$id, length(x$inputs))
  )
}

#' @export
.recipes_preserve_sparsity.step_slice <- function(x, ...) {
  TRUE
}
