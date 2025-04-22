#' Filter rows using dplyr
#'
#' `step_filter()` creates a *specification* of a recipe step that will remove
#' rows using [dplyr::filter()].
#'
#' @template row-ops
#' @inheritParams step_center
#' @param ... Logical predicates defined in terms of the variables in the data.
#'   Multiple conditions are combined with `&`. Only rows where the condition
#'   evaluates to `TRUE` are kept. See [dplyr::filter()] for more details.
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
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' The expressions in `terms` are text representations and are not parsable.
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
#'   step_filter(Sepal.Length > 4.5, Species == "setosa")
#'
#' prepped <- prep(rec, training = iris |> slice(1:75))
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris |>
#'   as_tibble() |>
#'   slice(1:75) |>
#'   dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris |>
#'   as_tibble() |>
#'   slice(76:150) |>
#'   dplyr::filter(Sepal.Length > 4.5, Species != "setosa")
#' rec_test <- bake(prepped, iris |> slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' values <- c("versicolor", "virginica")
#'
#' qq_rec <-
#'   recipe(~., data = iris) |>
#'   # Embed the `values` object in the call using !!
#'   step_filter(Sepal.Length > 4.5, Species %in% !!values)
#'
#' tidy(qq_rec, number = 1)
step_filter <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = TRUE,
  id = rand_id("filter")
) {
  inputs <- enquos(...)

  add_step(
    recipe,
    step_filter_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_filter_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "filter",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_filter <- function(x, training, info = NULL, ...) {
  step_filter_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_filter <- function(object, new_data, ...) {
  dplyr::filter(new_data, !!!object$inputs)
}

#' @export
print.step_filter <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Row filtering using "
    print_step(x$inputs, x$inputs, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_filter <- function(x, ...) {
  cond_expr <- map(unname(x$inputs), quo_get_expr)
  cond_expr <- map_chr(cond_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = cond_expr,
    id = rep(x$id, length(x$inputs))
  )
}

#' @export
.recipes_preserve_sparsity.step_filter <- function(x, ...) {
  TRUE
}
