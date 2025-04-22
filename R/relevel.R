#' Relevel factors to a desired level
#'
#' `step_relevel()` creates a *specification* of a recipe step that will reorder
#' the provided factor columns so that the level specified by `ref_level` is
#' first. This is useful for [contr.treatment()] contrasts which take the first
#' level as the reference.
#'
#' @inheritParams step_center
#' @param ref_level A single character value that will be used to relevel the
#'   factor column(s) (if the level is present).
#' @param objects A list of objects that contain the information on factor
#'   levels that will be determined by [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' The selected variables are releveled to a level (given by `ref_level`),
#' placing the `ref_level` in the first position.
#'
#' Note that if the original columns are character, they will be converted to
#' factors by this step.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, the value of `ref_level`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#' rec <- recipe(~ city + zip, data = Sacramento) |>
#'   step_unknown(city, new_level = "UNKNOWN") |>
#'   step_relevel(city, ref_level = "UNKNOWN") |>
#'   prep()
#'
#' data <- bake(rec, Sacramento)
#' levels(data$city)
step_relevel <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    ref_level,
    objects = NULL,
    skip = FALSE,
    id = rand_id("relevel")
  ) {
    add_step(
      recipe,
      step_relevel_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        ref_level = ref_level,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_relevel_new <-
  function(terms, role, trained, ref_level, objects, skip, id) {
    step(
      subclass = "relevel",
      terms = terms,
      role = role,
      trained = trained,
      ref_level = ref_level,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_relevel <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor"))
  check_string(x$ref_level, arg = "ref_level", allow_empty = FALSE)

  # Get existing levels and their factor type (i.e. ordered)
  objects <- lapply(training[, col_names], get_existing_values)
  # Check to make sure that no ordered levels are provided
  order_check <- map_lgl(objects, attr, "is_ordered")

  # Check to make sure that the reference level exists in the factor
  ref_check <- map_lgl(objects, function(x, y) !y %in% x, y = x$ref_level)
  if (any(ref_check)) {
    offenders <- names(order_check)[!order_check]
    cli::cli_abort(
      "{cli::qty(length(offenders))}The following column{?s} doesn't include \\
      required reference level {.val {x$ref_level}}: {.var {offenders}}."
    )
  }

  step_relevel_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ref_level = x$ref_level,
    objects = objects,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_relevel <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- stats::relevel(
      as.factor(new_data[[col_name]]),
      ref = object$ref_level
    )
  }

  new_data
}

#' @export
print.step_relevel <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Re-order factor level to ref_level for "
    print_step(names(x$objects), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_relevel <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$objects),
      value = x$ref_level
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = x$ref_level
    )
  }
  res$id <- x$id
  res
}
