#' Scaling Numeric Data to a Specific Range
#'
#' `step_range` creates a *specification* of a recipe
#'  step that will normalize numeric data to be within a pre-defined
#'  range of values.
#'
#' @inheritParams step_center
#' @param min A single numeric value for the smallest value in the
#'  range.
#' @param max A single numeric value for the largest value in the
#'  range.
#' @param clipping A single logical value for determining whether
#'  application of transformation onto new data should be forced
#'  to be inside `min` and `max`. Defaults to TRUE.
#' @param ranges A character vector of variables that will be
#'  normalized. Note that this is ignored until the values are
#'  determined by [prep()]. Setting this value will
#'  be ineffective.
#' @template step-return
#' @family normalization steps
#' @export
#' @details When a new data point is outside of the ranges seen in
#'  the training set, the new values are truncated at `min` or
#'  `max`.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or variables selected), `min`, and `max` is
#'  returned.
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' ranged_trans <- rec %>%
#'   step_range(carbon, hydrogen)
#'
#' ranged_obj <- prep(ranged_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ranged_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#'
#' tidy(ranged_trans, number = 1)
#' tidy(ranged_obj, number = 1)
step_range <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           min = 0,
           max = 1,
           clipping = TRUE,
           ranges = NULL,
           skip = FALSE,
           id = rand_id("range")) {
    add_step(
      recipe,
      step_range_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        min = min,
        max = max,
        clipping = clipping,
        ranges = ranges,
        skip = skip,
        id = id
      )
    )
  }

step_range_new <-
  function(terms, role, trained, min, max, clipping, ranges, skip, id) {
    step(
      subclass = "range",
      terms = terms,
      role = role,
      trained = trained,
      min = min,
      max = max,
      clipping = clipping,
      ranges = ranges,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_range <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  mins <-
    vapply(training[, col_names], min, c(min = 0), na.rm = TRUE)
  maxs <-
    vapply(training[, col_names], max, c(max = 0), na.rm = TRUE)
  step_range_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    min = x$min,
    max = x$max,
    clipping = x$clipping,
    ranges = rbind(mins, maxs),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_range <- function(object, new_data, ...) {
  check_new_data(colnames(object$ranges), object, new_data)

  for (column in colnames(object$ranges)) {
    min <- object$ranges["mins", column]
    max <- object$ranges["maxs", column]

    new_data[[column]] <- (new_data[[column]] - min) *
      (object$max - object$min) / (max - min) + object$min

    if (!is.null(object$clipping) && object$clipping) {
      new_data[[column]] <- pmax(new_data[[column]], object$min)
      new_data[[column]] <- pmin(new_data[[column]], object$max)
    }
  }
  new_data
}

print.step_range <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- glue::glue("Range scaling to [{x$min},{x$max}] for ")
    print_step(colnames(x$ranges), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = colnames(x$ranges) %||% character(),
      min = unname(x$ranges["mins", ]),
      max = unname(x$ranges["maxs", ])
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      min = na_dbl,
      max = na_dbl
    )
  }
  res$id <- x$id
  res
}
