#' Add new variables using dplyr
#'
#' `step_mutate()` creates a *specification* of a recipe step that will add
#' variables using [dplyr::mutate()].
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... Name-value pairs of expressions. See [dplyr::mutate()].
#' @param .pkgs Character vector, package names of functions used in expressions
#'   `...`. Should be specified if using non-base functions.
#' @param inputs Quosure(s) of `...`.
#' @template step-return
#' @template mutate-leakage
#' @details
#'
#' When an object in the user's global environment is referenced in the
#' expression defining the new variable(s), it is a good idea to use
#' quasiquotation (e.g. `!!`) to embed the value of the object in the expression
#' (to be portable between sessions). See the examples.
#'
#' If a preceding step removes a column that is selected by name in
#' `step_mutate()`, the recipe will error when being estimated with [prep()].
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, expression passed to `mutate()`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @family individual transformation steps
#' @family dplyr steps
#' @export
#' @examples
#' rec <-
#'   recipe(~., data = iris) |>
#'   step_mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#'
#' prepped <- prep(rec, training = iris |> slice(1:75))
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris |>
#'   as_tibble() |>
#'   slice(1:75) |>
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris |>
#'   as_tibble() |>
#'   slice(76:150) |>
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#' rec_test <- bake(prepped, iris |> slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' # Embedding objects:
#' const <- 1.414
#'
#' qq_rec <-
#'   recipe(~., data = iris) |>
#'   step_mutate(
#'     bad_approach = Sepal.Width * const,
#'     best_approach = Sepal.Width * !!const
#'   ) |>
#'   prep(training = iris)
#'
#' bake(qq_rec, new_data = NULL, contains("appro")) |> slice(1:4)
#'
#' # The difference:
#' tidy(qq_rec, number = 1)
#'
#' # Using across()
#' recipe(~., data = iris) |>
#'   step_mutate(across(contains("Length"), .fns = ~ 1 / .)) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:10)
#'
#' recipe(~., data = iris) |>
#'   # leads to more columns being created.
#'   step_mutate(
#'     across(contains("Length"), .fns = list(log = log, sqrt = sqrt))
#'   ) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:10)
step_mutate <- function(
  recipe,
  ...,
  .pkgs = character(),
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate")
) {
  check_character(.pkgs)
  recipes_pkg_check(required_pkgs.step_mutate(list(.pkgs = .pkgs)))

  inputs <- enquos(...)

  add_step(
    recipe,
    step_mutate_new(
      .pkgs = .pkgs,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_mutate_new <-
  function(.pkgs, role, trained, inputs, skip, id) {
    step(
      subclass = "mutate",
      .pkgs = .pkgs,
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
    trained = TRUE,
    .pkgs = x$.pkgs,
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

#' @export
print.step_mutate <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Variable mutation for "
    print_step(x$inputs, x$inputs, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_mutate <- function(x, ...) {
  inputs <- x$inputs

  terms <- names(quos_auto_name(inputs))
  value <- map_chr(unname(inputs), as_label)

  tibble(
    terms = terms,
    value = value,
    id = rep(x$id, length(x$inputs))
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_mutate <- function(x, ...) {
  x$.pkgs
}
