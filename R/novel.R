#' Simple Value Assignments for Novel Factor Levels
#'
#' `step_novel` creates a *specification* of a recipe
#'  step that will assign a previously unseen factor level to a
#'  new value.
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
#'  position. During preparation there will be no data points
#'  associated with this new level since all of the data have been
#'  seen.
#'
#' Note that if the original columns are character, they will be
#'  converted to factors by this step.
#'
#' Missing values will remain missing.
#'
#' If `new_level` is already in the data given to `prep`, an error
#'  is thrown.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' okc_tr <- okc[1:30000,]
#' okc_te <- okc[30001:30006,]
#' okc_te$diet[3] <- "cannibalism"
#' okc_te$diet[4] <- "vampirism"
#'
#' rec <- recipe(~ diet + location, data = okc_tr)
#'
#' rec <- rec %>%
#'   step_novel(diet, location)
#' rec <- prep(rec, training = okc_tr)
#'
#' processed <- bake(rec, okc_te)
#' tibble(old = okc_te$diet, new = processed$diet)
#'
#' tidy(rec, number = 1)

step_novel <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           new_level = "new",
           objects = NULL,
           skip = FALSE,
           id = rand_id("novel")) {
    add_step(
      recipe,
      step_novel_new(
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

step_novel_new <-
  function(terms, role, trained, new_level, objects, skip, id) {
    step(
      subclass = "novel",
      terms = terms,
      role = role,
      trained = trained,
      new_level = new_level,
      objects = objects,
      skip = skip,
      id = id
    )
  }

get_existing_values <- function(x) {
  if (is.character(x)) {
    out <- unique(x)
    attr(out, "is_ordered") <- FALSE
  } else {
    if (is.factor(x)) {
      out <- levels(x)
      attr(out, "is_ordered") <- is.ordered(x)
    }
    else
      stop("Data should be either character or factor", call. = FALSE)
  }
  out
}

#' @export
prep.step_novel <- function(x, training, info = NULL, ...) {
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
      "Columns already contain the new level: ",
      paste0(names(level_check)[level_check], collapse = ", "),
      call. = FALSE
    )

  step_novel_new(
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
bake.step_novel <- function(object, new_data, ...) {
  for (i in names(object$objects)) {
    new_data[[i]] <- ifelse(
      # Preserve NA values by adding them to the list of existing
      # possible values
      !(new_data[[i]] %in% c(object$object[[i]], NA)),
      object$new_level,
      as.character(new_data[[i]])
    )

    new_data[[i]] <-
      factor(new_data[[i]],
             levels = c(object$object[[i]], object$new_level),
             ordered = attributes(object$object[[i]])$is_ordered)
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_novel <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Novel factor level assignment for ", sep = "")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_novel
#' @param x A `step_novel` object.
#' @export
tidy.step_novel <- function(x, ...) {
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

