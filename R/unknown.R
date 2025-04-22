#' Assign missing categories to "unknown"
#'
#' `step_unknown()` creates a *specification* of a recipe step that will assign
#' a missing value in a factor level to `"unknown"`.
#'
#' @inheritParams step_center
#' @param new_level A single character value that will be assigned to new factor
#'   levels.
#' @param objects A list of objects that contain the information on factor
#'   levels that will be determined by [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_names()]
#' @export
#' @details
#'
#' The selected variables are adjusted to have a new level (given by
#' `new_level`) that is placed in the last position.
#'
#' Note that if the original columns are character, they will be converted to
#' factors by this step.
#'
#' If `new_level` is already in the data given to [prep()], an error is thrown.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{statistic}{character, the factor levels for the new values}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' rec <-
#'   recipe(~ city + zip, data = Sacramento) |>
#'   step_unknown(city, new_level = "unknown city") |>
#'   step_unknown(zip, new_level = "unknown zip") |>
#'   prep()
#'
#' table(bake(rec, new_data = NULL) |> pull(city),
#'   Sacramento |> pull(city),
#'   useNA = "always"
#' ) |>
#'   as.data.frame() |>
#'   dplyr::filter(Freq > 0)
#'
#' tidy(rec, number = 1)
step_unknown <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    new_level = "unknown",
    objects = NULL,
    skip = FALSE,
    id = rand_id("unknown")
  ) {
    add_step(
      recipe,
      step_unknown_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        new_level = new_level,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_unknown_new <-
  function(terms, role, trained, new_level, objects, skip, id) {
    step(
      subclass = "unknown",
      terms = terms,
      role = role,
      trained = trained,
      new_level = new_level,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_unknown <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))
  check_string(x$new_level, arg = "new_level", allow_empty = FALSE)

  # Get existing levels and their factor type (i.e. ordered)
  objects <- lapply(training[, col_names], get_existing_values)
  # Check to make sure that there are not duplicate levels
  level_check <-
    map_lgl(objects, function(x, y) y %in% x, y = x$new_level)
  if (any(level_check)) {
    offenders <- names(level_check)[level_check]
    cli::cli_abort(
      "Columns already contain the level {.val {x$new_level}}: {offenders}."
    )
  }

  step_unknown_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    new_level = x$new_level,
    objects = objects,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_unknown <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- ifelse(
      is.na(new_data[[col_name]]),
      object$new_level,
      as.character(new_data[[col_name]])
    )

    new_levels <- c(object$object[[col_name]], object$new_level)

    if (!all(new_data[[col_name]] %in% new_levels)) {
      warn_new_levels(
        new_data[[col_name]],
        new_levels,
        column = col_name,
        step = "step_unknown",
        c("*" = "New levels will be coerced to `NA` by {.fn step_unknown}.")
      )
    }

    new_data[[col_name]] <-
      factor(
        new_data[[col_name]],
        levels = new_levels,
        ordered = attributes(object$object[[col_name]])$is_ordered
      )
  }
  new_data
}

#' @export
print.step_unknown <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Unknown factor level assignment for "
    print_step(names(x$objects), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_unknown <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$objects),
      value = rep(x$new_level, length(x$objects))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = rep(x$new_level, length(term_names))
    )
  }
  res$id <- x$id
  res
}
