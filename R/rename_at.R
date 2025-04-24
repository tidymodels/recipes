#' Rename multiple columns using dplyr
#'
#' `step_rename_at()` creates a *specification* of a recipe step that will
#' rename the selected variables using a common function via
#' [dplyr::rename_at()].
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param fn A function `fun`, a quosure style lambda `~ fun(.)` or a list of
#'   either form (but containing only a single function, see
#'   [dplyr::rename_at()]).
#' **Note that this argument must be named**.
#' @param inputs A vector of column names populated by [prep()].
#' @template step-return
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
#' @template sparse-preserve
#'
#' @template case-weights-not-supported
#'
#' @family dplyr steps
#' @export
#' @examples
#' library(dplyr)
#' recipe(~., data = iris) |>
#'   step_rename_at(all_predictors(), fn = ~ gsub(".", "_", ., fixed = TRUE)) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:10)
#' @export
step_rename_at <- function(
  recipe,
  ...,
  fn,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("rename_at")
) {
  if (rlang::is_missing(fn)) {
    cli::cli_abort("Argument {.arg fn} must be specified.")
  }

  add_step(
    recipe,
    step_rename_at_new(
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

step_rename_at_new <-
  function(terms, fn, role, trained, inputs, skip, id) {
    step(
      subclass = "rename_at",
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
prep.step_rename_at <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  step_rename_at_new(
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
bake.step_rename_at <- function(object, new_data, ...) {
  dplyr::rename_at(new_data, .vars = object$inputs, .funs = object$fn)
}

#' @export
print.step_rename_at <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Variable renaming for "
    print_step(x$inputs, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_rename_at <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$inputs))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_rename_at <- function(x, ...) {
  TRUE
}
