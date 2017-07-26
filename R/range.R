#' Scaling Numeric Data to a Specific Range
#'
#' \code{step_range} creates a \emph{specification} of a recipe step that will
#'   normalize numeric data to have a standard deviation of one.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'   scaled. See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param min A single numeric value for the smallest value in the range
#' @param max A single numeric value for the largest value in the range
#' @param ranges A character vector of variables that will be normalized. Note
#'   that this is ignored until the values are determined by
#'   \code{\link{prep.recipe}}. Setting this value will be ineffective.
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details Scaling data means that the standard deviation of a variable is
#'   divided out of the data. \code{step_range} estimates the variable standard
#'   deviations from the data used in the \code{training} argument of
#'   \code{prep.recipe}. \code{bake.recipe} then applies the scaling to new
#'   data sets using these standard deviations.
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

step_range <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           min = 0,
           max = 1,
           ranges = NULL) {
    add_step(
      recipe,
      step_range_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        min = min,
        max = max,
        ranges = ranges
      )
    )
  }

step_range_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           min = 0,
           max = 1,
           ranges = NULL) {
    step(
      subclass = "range",
      terms = terms,
      role = role,
      trained = trained,
      min = min,
      max = max,
      ranges = ranges
    )
  }

#' @importFrom stats sd
#' @export
prep.step_range <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
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
    ranges = rbind(mins, maxs)
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
    printer(names(x$ranges), x$terms, x$trained, width = width)
    invisible(x)
  }
