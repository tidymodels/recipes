#' Scaling Numeric Data
#'
#' \code{step_scale} creates a \emph{specification} of a recipe step that
#'   will normalize numeric data to have a standard deviation of one.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param sds A named numeric vector of standard deviations This is \code{NULL}
#'   until computed by \code{\link{prep.recipe}}.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   removed when computing the standard deviation.
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details Scaling data means that the standard deviation of a variable is
#'   divided out of the data. \code{step_scale} estimates the variable
#'   standard deviations from the data used in the \code{training} argument of
#'   \code{prep.recipe}. \code{bake.recipe} then applies the scaling to
#'   new data sets using these standard deviations.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' scaled_trans <- rec %>%
#'   step_scale(carbon, hydrogen)
#'
#' scaled_obj <- prep(scaled_trans, training = biomass_tr)
#'
#' transformed_te <- bake(scaled_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te

step_scale <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           sds = NULL,
           na.rm = TRUE) {
    add_step(
      recipe,
      step_scale_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        sds = sds,
        na.rm = na.rm
      )
    )
  }

step_scale_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           sds = NULL,
           na.rm = NULL) {
    step(
      subclass = "scale",
      terms = terms,
      role = role,
      trained = trained,
      sds = sds,
      na.rm = na.rm
    )
  }

#' @importFrom stats sd
#' @export
prep.step_scale <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  sds <-
    vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na.rm)
  step_scale_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    sds,
    na.rm = x$na.rm
  )
}

#' @export
bake.step_scale <- function(object, newdata, ...) {
  res <-
    sweep(as.matrix(newdata[, names(object$sds)]), 2, object$sds, "/")
  if (is.matrix(res) && ncol(res) == 1)
    res <- res[, 1]
  newdata[, names(object$sds)] <- res
  as_tibble(newdata)
}

print.step_scale <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Scaling for ", sep = "")
    printer(names(x$sds), x$terms, x$trained, width = width)
    invisible(x)
  }
