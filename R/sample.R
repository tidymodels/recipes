#' Sample rows using dplyr
#'
#' `step_sample` creates a *specification* of a recipe step
#'  that will sample rows using [dplyr::sample_n()] or
#'  [dplyr::sample_frac()].
#'
#' @template row-ops
#' @inheritParams step_center
#' @param ... Argument ignored; included for consistency with other step
#'  specification functions.
#' @param size An integer or fraction. If the value is within (0, 1),
#'  [dplyr::sample_frac()] is applied to the data. If an integer
#'  value of 1 or greater is used, [dplyr::sample_n()] is applied.
#'  The default of `NULL` uses [dplyr::sample_n()] with the size
#'  of the training set (or smaller for smaller `new_data`).
#' @param replace Sample with or without replacement?
#' @template step-return
#' @details When you [`tidy()`] this step, a tibble with columns `size`,
#' `replace`, and `id` is returned.
#' @family row operation steps
#' @family dplyr steps
#' @export
#' @examples
#'
#' # Uses `sample_n`
#' recipe( ~ ., data = mtcars) %>%
#'   step_sample(size = 1) %>%
#'   prep(training = mtcars) %>%
#'   bake(new_data = NULL) %>%
#'   nrow()
#'
#' # Uses `sample_frac`
#' recipe( ~ ., data = mtcars) %>%
#'   step_sample(size = 0.9999) %>%
#'   prep(training = mtcars) %>%
#'   bake(new_data = NULL) %>%
#'   nrow()
#'
#' # Uses `sample_n` and returns _at maximum_ 20 samples.
#' smaller_cars <-
#'   recipe( ~ ., data = mtcars) %>%
#'   step_sample() %>%
#'   prep(training = mtcars %>% slice(1:20))
#'
#' bake(smaller_cars, new_data = NULL) %>% nrow()
#' bake(smaller_cars, new_data = mtcars %>% slice(21:32)) %>% nrow()
step_sample <- function(
  recipe, ...,
  role = NA,
  trained = FALSE,
  size = NULL,
  replace = FALSE,
  skip = TRUE,
  id = rand_id("sample")
) {

  if (length(list(...)) > 0) {
    rlang::warn("Selectors are not used for this step.")
  }

  if (!is_tune(size) & !is_varying(size)) {
    if (!is.null(size) & (!is.numeric(size) || size < 0)) {
      rlang::abort("`size` should be a positive number or NULL.")
    }
  }
  if (!is_tune(replace) & !is_varying(replace)) {
    if (!is.logical(replace)) {
      rlang::abort("`replace` should be a single logical.")
    }
  }

  add_step(
    recipe,
    step_sample_new(
      terms = terms,
      trained = trained,
      role = role,
      size = size,
      replace = replace,
      skip = skip,
      id = id
    )
  )
}

step_sample_new <-
  function(terms, role, trained, size, replace, skip, id) {
    step(
      subclass = "sample",
      terms = terms,
      role = role,
      trained = trained,
      size = size,
      replace = replace,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sample <- function(x, training, info = NULL, ...) {
  if (is.null(x$size)) {
    x$size <- nrow(training)
  }
  step_sample_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    size = x$size,
    replace = x$replace,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_sample <- function(object, new_data, ...) {
  if (object$size >= 1) {
    n <- min(object$size, nrow(new_data))
    new_data <-
      dplyr::sample_n(new_data, size = floor(n), replace = object$replace)
  } else {
    new_data <-
      dplyr::sample_frac(new_data, size = object$size, replace = object$replace)
  }
  new_data
}


print.step_sample <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Row sampling")
    if (x$replace)
      cat(" with replacement")
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_sample <- function(x, ...) {
  tibble(
    size = x$size,
    replace = x$replace,
    id = x$inputs
  )
}
