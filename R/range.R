#' Scaling Numeric Data to a Specific Range
#'
#' `step_range` creates a *specification* of a recipe
#'  step that will normalize numeric data to be within a pre-defined
#'  range of values.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables will be scaled. See [selections()] for more
#'  details. For the `tidy` method, these are not currently
#'  used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param min A single numeric value for the smallest value in the
#'  range
#' @param max A single numeric value for the largest value in the
#'  range
#' @param ranges A character vector of variables that will be
#'  normalized. Note that this is ignored until the values are
#'  determined by [prep.recipe()]. Setting this value will
#'  be ineffective.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `min`, and `max`.
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details When a new data point is outside of the ranges seen in
#'  the training set, the new values are truncated at `min` or
#'  `max`.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
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
           ranges = NULL,
           skip = FALSE) {
    add_step(
      recipe,
      step_range_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        min = min,
        max = max,
        ranges = ranges,
        skip = skip
      )
    )
  }

step_range_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           min = 0,
           max = 1,
           ranges = NULL,
           skip = FALSE) {
    step(
      subclass = "range",
      terms = terms,
      role = role,
      trained = trained,
      min = min,
      max = max,
      ranges = ranges,
      skip = skip
    )
  }

#' @importFrom stats sd
#' @export
prep.step_range <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

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
    ranges = rbind(mins, maxs),
    skip = x$skip
  )
}

#' @export
bake.step_range <- function(object, newdata, ...) {
  tmp <- as.matrix(newdata[, colnames(object$ranges)])
  tmp <- sweep(tmp, 2, object$ranges[1, ], "-")
  tmp <- tmp * (object$max - object$min)
  tmp <- sweep(tmp, 2, object$ranges[2, ] - object$ranges[1, ], "/")
  tmp <- tmp + object$min

  tmp[tmp < object$min] <- object$min
  tmp[tmp > object$max] <- object$max

  if (is.matrix(tmp) && ncol(tmp) == 1)
    tmp <- tmp[, 1]
  newdata[, colnames(object$ranges)] <- tmp
  as_tibble(newdata)
}

print.step_range <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Range scaling to [", x$min, ",", x$max, "] for ", sep = "")
    printer(colnames(x$ranges), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_range
#' @param x A `step_range` object.
tidy.step_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = colnames(x$ranges),
                  min = x$ranges["mins",],
                  max = x$ranges["maxs",])
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  min = na_dbl,
                  max = na_dbl)
  }
  res
}
