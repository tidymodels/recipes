#' Assign missing categories to "unknown"
#'
#' `step_unknown` creates a *specification* of a recipe
#'  step that will assign a missing value in a factor level to"unknown".
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables that will be affected by the step. These variables
#'  should be character or factor types. See [selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param new_level A single character value that will be assigned
#'  to new factor levels.
#' @param objects A list of objects that contain the information
#'  on factor levels that will be determined by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `value` (the factor
#'  levels that is used for the new value)
#' @keywords datagen
#' @concept preprocessing
#' @concept factors
#' @export
#' @details The selected variables are adjusted to have a new
#'  level (given by `new_level`) that is placed in the last
#'  position.
#'
#' Note that if the original columns are character, they will be
#'  converted to factors by this step.
#'
#' If `new_level` is already in the data given to `prep`, an error
#'  is thrown.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()], [step_novel()]
#' @examples
#' data(okc)
#'
#' rec <-
#'   recipe(~ diet + location, data = okc) %>%
#'   step_unknown(diet, new_level = "unknown diet") %>%
#'   step_unknown(location, new_level = "unknown location") %>%
#'   prep()
#'
#' table(juice(rec)$diet, okc$diet, useNA = "always") %>%
#'   as.data.frame() %>%
#'   dplyr::filter(Freq > 0)
#'
#' tidy(rec, number = 1)

step_unknown <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           new_level = "unknown",
           objects = NULL,
           skip = FALSE,
           id = rand_id("unknown")) {
    add_step(
      recipe,
      step_unknown_new(
        terms = ellipse_check(...),
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

#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#' @export
prep.step_unknown <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  col_check <- dplyr::filter(info, variable %in% col_names)
  if (any(col_check$type != "nominal"))
    stop(
      "Columns must be character or factor: ",
      paste0(col_check$variable[col_check$type != "nominal"],
             collapse = ", "),
      call. = FALSE
    )

  # Get existing levels and their factor type (i.e. ordered)
  objects <- lapply(training[, col_names], get_existing_values)
  # Check to make sure that there are not duplicate levels
  level_check <-
    map_lgl(objects, function(x, y) y %in% x, y = x$new_level)
  if (any(level_check))
    stop(
      "Columns already contain a level '", x$new_level, "': ",
      paste0(names(level_check)[level_check], collapse = ", "),
      call. = FALSE
    )

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

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_unknown <- function(object, new_data, ...) {
  for (i in names(object$objects)) {
    new_data[[i]] <-
      ifelse(is.na(new_data[[i]]), object$new_level, as.character(new_data[[i]]))

    new_data[[i]] <-
      factor(new_data[[i]],
             levels = c(object$object[[i]], object$new_level),
             ordered = attributes(object$object[[i]])$is_ordered)
  }
  if (!is_tibble(new_data)) {
    new_data <- as_tibble(new_data)
  }
  new_data
}

print.step_unknown <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Unknown factor level assignment for ", sep = "")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_unknown
#' @param x A `step_unknown` object.
#' @importFrom purrr map
#' @export
tidy.step_unknown <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$objects),
                  value = rep(x$new_level, length(x$objects)))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = rep(x$new_level, length(term_names)))
  }
  res$id <- x$id
  res
}
