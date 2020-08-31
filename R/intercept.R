#' Add intercept (or constant) column
#'
#' `step_intercept` creates a *specification* of a recipe step that
#'   will add an intercept or constant term in the first column of a data
#'   matrix. `step_intercept` has defaults to *predictor* role so
#'   that it is by default called in the bake step. Be careful to avoid
#'   unintentional transformations when calling steps with
#'   `all_predictors`.
#'
#' @inheritParams step_center
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... Argument ignored; included for consistency with other step
#'   specification functions.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created from the original variables will be
#'  used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param name Character name for newly added column
#' @param value A numeric constant to fill the intercept column. Defaults to 1.
#'
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#' rec_trans <- recipe(HHV ~ ., data = biomass_tr[, -(1:2)]) %>%
#'   step_intercept(value = 2) %>%
#'   step_scale(carbon)
#'
#' rec_obj <- prep(rec_trans, training = biomass_tr)
#'
#' with_intercept <- bake(rec_obj, biomass_te)
#' with_intercept
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_intercept <- function(recipe, ..., role = "predictor",
                           trained = FALSE, name = "intercept",
                           value = 1,
                           skip = FALSE, id = rand_id("intercept")) {
  if (length(list(...)) > 0)
    rlang::warn("Selectors are not used for this step.")
  if (!is.numeric(value))
    rlang::abort("Intercept value must be numeric.")
  if (!is.character(name) | length(name) != 1)
    rlang::abort("Intercept/constant column name must be a character value.")
  add_step(
    recipe,
    step_intercept_new(
      role = role,
      trained = trained,
      name = name,
      value = value,
      skip = skip,
      id = id
    )
  )
}

step_intercept_new <-
  function(role, trained, name, value, skip, id) {
    step(
      subclass = "intercept",
      role = role,
      trained = trained,
      name = name,
      value = value,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_intercept <- function(x, training, info = NULL, ...) {
  x$trained <- TRUE
  x
}

#' @export
bake.step_intercept <- function(object, new_data, ...) {
  tibble::add_column(new_data, !!object$name := object$value, .before = TRUE)
}

print.step_intercept <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Adding intercept\n")
    invisible(x)
  }
