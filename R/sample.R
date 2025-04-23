#' Sample rows using dplyr
#'
#' `step_sample()` creates a *specification* of a recipe step that will sample
#' rows using [dplyr::sample_n()] or [dplyr::sample_frac()].
#'
#' @template row-ops
#' @inheritParams step_center
#' @param ... Argument ignored; included for consistency with other step
#'   specification functions.
#' @param size An integer or fraction. If the value is within (0, 1),
#'   [dplyr::sample_frac()] is applied to the data. If an integer value of 1 or
#'   greater is used, [dplyr::sample_n()] is applied. The default of `NULL` uses
#'   [dplyr::sample_n()] with the size of the training set (or smaller for
#'   smaller `new_data`).
#' @param replace Sample with or without replacement?
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `size`, `replace` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{size}{numeric, amount of sampling}
#'   \item{replace}{logical, whether sampling is done with replacement}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-unsupervised
#'
#' @family row operation steps
#' @family dplyr steps
#' @export
#' @examples
#'
#' # Uses `sample_n`
#' recipe(~., data = mtcars) |>
#'   step_sample(size = 1) |>
#'   prep(training = mtcars) |>
#'   bake(new_data = NULL) |>
#'   nrow()
#'
#' # Uses `sample_frac`
#' recipe(~., data = mtcars) |>
#'   step_sample(size = 0.9999) |>
#'   prep(training = mtcars) |>
#'   bake(new_data = NULL) |>
#'   nrow()
#'
#' # Uses `sample_n` and returns _at maximum_ 20 samples.
#' smaller_cars <-
#'   recipe(~., data = mtcars) |>
#'   step_sample() |>
#'   prep(training = mtcars |> slice(1:20))
#'
#' bake(smaller_cars, new_data = NULL) |> nrow()
#' bake(smaller_cars, new_data = mtcars |> slice(21:32)) |> nrow()
step_sample <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  size = NULL,
  replace = FALSE,
  skip = TRUE,
  id = rand_id("sample")
) {
  if (length(list(...)) > 0) {
    cli::cli_warn("Selectors are not used for this step.")
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
      id = id,
      case_weights = NULL
    )
  )
}

step_sample_new <-
  function(terms, role, trained, size, replace, skip, id, case_weights) {
    step(
      subclass = "sample",
      terms = terms,
      role = role,
      trained = trained,
      size = size,
      replace = replace,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_sample <- function(x, training, info = NULL, ...) {
  check_number_decimal(x$size, min = 0, allow_null = TRUE, arg = "size")
  check_bool(x$replace, arg = "replace")
  if (is.null(x$size)) {
    x$size <- nrow(training)
  }

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  step_sample_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    size = x$size,
    replace = x$replace,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_sample <- function(object, new_data, ...) {
  if (isTRUE(object$case_weights)) {
    wts_col <- purrr::map_lgl(new_data, hardhat::is_case_weights)
    wts <- new_data[[names(which(wts_col))]]
    wts <- as.double(wts)
  } else {
    wts <- NULL
  }

  if (object$size >= 1) {
    n <- min(object$size, nrow(new_data))
    new_data <-
      dplyr::sample_n(
        new_data,
        size = floor(n),
        replace = object$replace,
        weight = wts
      )
  } else {
    new_data <-
      dplyr::sample_frac(
        new_data,
        size = object$size,
        replace = object$replace,
        weight = wts
      )
  }
  new_data
}

#' @export
print.step_sample <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Row sampling "
    if (x$replace) {
      title <- paste(title, "with replacement ")
    }
    print_step(
      NULL,
      NULL,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_sample <- function(x, ...) {
  if (is.null(x$size)) {
    res <- tibble(size = numeric(), replace = logical())
  } else {
    res <- tibble(
      size = x$size,
      replace = x$replace
    )
  }
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_sample <- function(x, ...) {
  TRUE
}
