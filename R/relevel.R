#' Relevel factors to a desired level
#'
#' `step_relevel` creates a *specification* of a recipe
#'  step that will reorder the provided factor columns so that
#'  the level specified by ref_level is first. This is useful
#'  for contr.treatment contrasts which take the first level as the
#'  reference.
#'
#' @inheritParams step_center
#' @inherit step_center return
#'
#' @param ... One or more selector functions to choose which
#'  variables that will be affected by the step. These variables
#'  should be character or factor types. See [selections()] for more
#'  details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param ref_level A single character value that will be used to
#'  relevel the factor column(s) (if the level is present).
#' @param objects A list of objects that contain the information
#'  on factor levels that will be determined by [prep.recipe()].
#' @template step-return
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept factors
#' @export
#' @details The selected variables are releveled to a level
#' (given by `ref_level`). Placing the `ref_level` in the first
#' position.
#'
#' Note that if the original columns are character, they will be
#'  converted to factors by this step.
#'
#'
#' @examples
#'
#' library(modeldata)
#' data(okc)
#' rec <- recipe(~ diet + location, data = okc) %>%
#'   step_unknown(diet, new_level = "UNKNOWN") %>%
#'   step_relevel(diet, ref_level = "UNKNOWN") %>%
#'   prep()
#'
#' data <- bake(rec, okc)
#' levels(data$diet)
step_relevel <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           ref_level,
           objects = NULL,
           skip = FALSE,
           id = rand_id("relevel")) {
    add_step(
      recipe,
      step_relevel_new(
        terms = ellipse_check(...),
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
  col_names <- eval_select_recipes(x$terms, training, info)

  col_check <- dplyr::filter(info, .data$variable %in% col_names)

  if (any(col_check$type != "nominal")) {
    rlang::abort(
      "Columns must be character or factor: ",
      paste0(col_check$variable[col_check$type != "nominal"],
        collapse = ", "
      )
    )
  }

  # Get existing levels and their factor type (i.e. ordered)
  objects <- lapply(training[, col_names], get_existing_values)
  # Check to make sure that no ordered levels are provided
  order_check <- map_lgl(objects, attr, "is_ordered")
  if (any(order_check)) {
    rlang::abort(
      "Columns contain ordered factors (which cannot be releveled) '",
      x$ref_level, "': ",
      paste0(names(order_check)[order_check], collapse = ", ")
    )
  }

  # Check to make sure that the reference level exists in the factor
  ref_check <- map_lgl(objects, function(x, y) !y %in% x,
    y = x$ref_level
  )
  if (any(ref_check)) {
    rlang::abort(
      "Columns must contain the reference level '",
      x$ref_level, "': ",
      paste0(names(ref_check)[ref_check], collapse = ", ")
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
  for (i in names(object$objects)) {
    new_data[[i]] <- stats::relevel(as.factor(new_data[[i]]), ref = object$ref_level)
  }
  if (!is_tibble(new_data)) {
    new_data <- as_tibble(new_data)
  }
  new_data
}

print.step_relevel <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Re-order factor level to ref_level for ", sep = "")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_relevel` object.
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
