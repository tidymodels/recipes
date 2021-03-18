#' Mutate multiple columns using dplyr
#'
#' `step_mutate_at` creates a *specification* of a recipe step that will modify
#' the selected variables using a common function via [dplyr::mutate_at()].
#'
#' @inheritParams step_center
#' @param fn A function fun, a quosure style lambda `~ fun(.)`` or a list of
#' either form. (see [dplyr::mutate_at()]). **Note that this argument must be
#' named**.
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
#'   step_mutate_at(contains("Length"), fn = ~ 1/.) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   slice(1:10)
#'
#' recipe(~ ., data = iris) %>%
#'   # leads to more columns being created.
#'   step_mutate_at(contains("Length"), fn = list(log = log, sqrt = sqrt)) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
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
  col_names <- eval_select_recipes(x$terms, training, info)

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
    printer(x$inputs, x$terms, x$trained, width = width)
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

