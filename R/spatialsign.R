#' Spatial Sign Preprocessing
#'
#' `step_spatialsign` is a *specification* of a recipe
#'  step that will convert numeric data into a projection on to a
#'  unit sphere.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used for the normalization. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?
#' @param na_rm A logical: should missing data be removed from the
#'  norm computation?
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @keywords datagen
#' @concept preprocessing
#' @concept projection_methods
#' @export
#' @details The spatial sign transformation projects the variables
#'  onto a unit sphere and is related to global contrast
#'  normalization. The spatial sign of a vector `w` is
#'  `w/norm(w)`.
#'
#' The variables should be centered and scaled prior to the
#'  computations.
#'
#' @references Serneels, S., De Nolf, E., and Van Espen, P.
#'  (2006). Spatial sign preprocessing: a simple way to impart
#'  moderate robustness to multivariate estimators. *Journal of
#'  Chemical Information and Modeling*, 46(3), 1402-1409.

#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' ss_trans <- rec %>%
#'   step_center(carbon, hydrogen) %>%
#'   step_scale(carbon, hydrogen) %>%
#'   step_spatialsign(carbon, hydrogen)
#'
#' ss_obj <- prep(ss_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ss_obj, biomass_te)
#'
#' plot(biomass_te$carbon, biomass_te$hydrogen)
#'
#' plot(transformed_te$carbon, transformed_te$hydrogen)
#'
#' tidy(ss_trans, number = 3)
#' tidy(ss_obj, number = 3)

step_spatialsign <-
  function(recipe,
           ...,
           role = "predictor",
           na_rm = TRUE,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("spatialsign")) {
    add_step(recipe,
             step_spatialsign_new(
               terms = ellipse_check(...),
               role = role,
               na_rm = na_rm,
               trained = trained,
               columns = columns,
               skip = skip,
               id = id
             ))
  }

step_spatialsign_new <-
  function(terms, role, na_rm, trained, columns, skip, id) {
    step(
      subclass = "spatialsign",
      terms = terms,
      role = role,
      na_rm = na_rm,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_spatialsign <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_type(training[, col_names])

  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    na_rm = x$na_rm,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_spatialsign <- function(object, new_data, ...) {
  col_names <- object$columns
  ss <- function(x, na_rm) {
    x / sqrt(sum(x ^ 2, na.rm = na_rm))
  }
  res <- t(apply(as.matrix(new_data[, col_names]), 1, ss, na_rm = object$na_rm))
  res <- tibble::as_tibble(res)
  new_data[, col_names] <- res
  tibble::as_tibble(new_data)
}

print.step_spatialsign <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Spatial sign on  ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_spatialsign
#' @param x A `step_spatialsign` object.
#' @export
tidy.step_spatialsign <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}
