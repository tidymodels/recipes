#' Scaling Numeric Data
#'
#' `step_scale` creates a *specification* of a recipe
#'  step that will normalize numeric data to have a standard
#'  deviation of one.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param sds A named numeric vector of standard deviations. This
#'  is `NULL` until computed by [prep.recipe()].
#' @param factor A numeric value of either 1 or 2 that scales the
#'  numeric inputs by one or two standard deviations. By dividing
#'  by two standard deviations, the coefficients attached to
#'  continuous predictors can be interpreted the same way as with
#'  binary inputs. Defaults to `1`. More in reference below.
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed when computing the standard deviation.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  standard deviations).
#' @keywords datagen
#' @concept preprocessing
#' @concept normalization_methods
#' @export
#' @details Scaling data means that the standard deviation of a
#'  variable is divided out of the data. `step_scale` estimates
#'  the variable standard deviations from the data used in the
#'  `training` argument of `prep.recipe`.
#'  `bake.recipe` then applies the scaling to new data sets
#'  using these standard deviations.
#' @references Gelman, A. (2007) "Scaling regression inputs by
#'  dividing by two standard deviations." Unpublished. Source:
#'  \url{http://www.stat.columbia.edu/~gelman/research/unpublished/standardizing.pdf}.
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
#' scaled_trans <- rec %>%
#'   step_scale(carbon, hydrogen)
#'
#' scaled_obj <- prep(scaled_trans, training = biomass_tr)
#'
#' transformed_te <- bake(scaled_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' tidy(scaled_trans, number = 1)
#' tidy(scaled_obj, number = 1)
#'
step_scale <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           sds = NULL,
           factor = 1,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("scale")) {
    add_step(
      recipe,
      step_scale_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        sds = sds,
        factor = factor,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_scale_new <-
  function(terms, role, trained, sds, factor, na_rm, skip, id) {
    step(
      subclass = "scale",
      terms = terms,
      role = role,
      trained = trained,
      sds = sds,
      factor = factor,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_scale <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)
  check_type(training[, col_names])

  if (x$factor != 1 & x$factor != 2) {
    rlang::warn("Scaling `factor` should take either a value of 1 or 2")
  }

  sds <-
    vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na_rm)

  sds <- sds * x$factor

  step_scale_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    sds,
    factor = x$factor,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_scale <- function(object, new_data, ...) {
  res <-
    sweep(as.matrix(new_data[, names(object$sds)]), 2, object$sds, "/")
  res <- tibble::as_tibble(res)
  new_data[, names(object$sds)] <- res
  as_tibble(new_data)
}

print.step_scale <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Scaling for ", sep = "")
    printer(names(x$sds), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_scale
#' @param x A `step_scale` object.
#' @export
tidy.step_scale <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$sds),
                  value = x$sds)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}
