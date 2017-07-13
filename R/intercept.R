#' Add intercept (or constant) column
#'
#' \code{step_intercept} creates a \emph{specification} of a recipe step that
#'   will add an intercept or constant term in the first column of a data
#'   matrix. \code{step_intercept} has \emph{predictor} role so that it is
#'   by default called in the bake step. This means you should always call
#'   \code{step_intercept} last to avoid unintentional transformations such as
#'   centering, scaling, etc.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... Argument ignored; included for consistency with other step
#'   specification functions.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param value A numeric constant to fill the intercept column. Defaults to 1.
#' @return An updated version of \code{recipe} with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#' rec_trans <- recipe(HHV ~ ., data = biomass_tr[, -(1:2)])
#' rec_trans <- step_intercept(rec_trans, value = 2)
#'
#' rec_obj <- prepare(rec_trans, training = biomass_tr)
#'
#' with_intercept <- bake(rec_obj, biomass_te)
#' with_intercept
#'
#' @seealso \code{\link{recipe}} \code{\link{prepare.recipe}}
#'   \code{\link{bake.recipe}}
step_intercept <- function(recipe, ..., role = "predictor",
                           trained = FALSE, value = 1) {
  if(length(list(...)) > 0)
    warning("Term arguments passed to step_intercept have no effect.")
  if (!is.numeric(value))
    stop("Intercept value must be numeric.")
  add_step(
    recipe,
    step_intercept_new(
      role = role,
      trained = trained,
      value = value))
}

step_intercept_new <- function(role = "predictor", trained = FALSE, value = 1) {
  step(
    subclass = "intercept",
    role = role,
    trained = trained,
    value = value
  )
}

prepare.step_intercept <- function(x, training, info = NULL, ...) {
  x
}

bake.step_intercept <- function(object, newdata, ...) {
  if (all(newdata[1, ] == 1))
    message("Data appears to already contain intercept column.")
  tibble::add_column(newdata, intercept = object$value, .before = TRUE)
}
