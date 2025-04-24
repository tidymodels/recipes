#' Mutate multiple columns using dplyr
#'
#' @description `r lifecycle::badge("superseded")`
#'
#'   `step_mutate_at()` is superseded in favor of using [step_mutate()] with
#'   [dplyr::across()].
#'
#'   `step_mutate_at()` creates a *specification* of a recipe step that will
#'   modify the selected variables using a common function via
#'   [dplyr::mutate_at()].
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param fn A function fun, a quosure style lambda `~ fun(.)` or a list of
#'   either form. (see [dplyr::mutate_at()]). **Note that this argument must be
#'   named**.
#' @param inputs A vector of column names populated by [prep()].
#' @template step-return
#' @template mutate-leakage
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
#' @template case-weights-not-supported
#'
#' @family multivariate transformation steps
#' @family dplyr steps
#' @export
#' @examples
#' library(dplyr)
#' recipe(~., data = iris) |>
#'   step_mutate_at(contains("Length"), fn = ~ 1 / .) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:10)
#'
#' recipe(~., data = iris) |>
#'   # leads to more columns being created.
#'   step_mutate_at(contains("Length"), fn = list(log = log, sqrt = sqrt)) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:10)
#' @export
step_mutate_at <- function(
  recipe,
  ...,
  fn,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("mutate_at")
) {
  lifecycle::signal_stage("superseded", "step_mutate_at()", "step_mutate()")

  if (rlang::is_missing(fn)) {
    cli::cli_abort("Argument {.arg fn} must be specified.")
  }

  add_step(
    recipe,
    step_mutate_at_new(
      terms = enquos(...),
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
  col_names <- recipes_eval_select(x$terms, training, info)

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

#' @export
print.step_mutate_at <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Variable mutation for "
    print_step(x$inputs, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_mutate_at <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$inputs))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
