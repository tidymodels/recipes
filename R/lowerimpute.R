#' Impute Numeric Data Below the Threshold of Measurement
#'
#' `step_lowerimpute` creates a *specification* of a recipe step
#'  designed for cases where the non-negative numeric data cannot be
#'  measured below a known value. In these cases, one method for
#'  imputing the data is to substitute the truncated value by a
#'  random uniform number between zero and the truncation point.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param threshold A named numeric vector of lower bounds. This is
#'  `NULL` until computed by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` for the estimated
#'  threshold.
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details `step_lowerimpute` estimates the variable minimums
#'  from the data used in the `training` argument of `prep.recipe`.
#'  `bake.recipe` then simulates a value for any data at the minimum
#'  with a random uniform value between zero and the minimum.
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(biomass)
#'
#' ## Truncate some values to emulate what a lower limit of
#' ## the measurement system might look like
#'
#' biomass$carbon <- ifelse(biomass$carbon > 40, biomass$carbon, 40)
#' biomass$hydrogen <- ifelse(biomass$hydrogen > 5, biomass$carbon, 5)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' impute_rec <- rec %>%
#'   step_lowerimpute(carbon, hydrogen)
#'
#' tidy(impute_rec, number = 1)
#'
#' impute_rec <- prep(impute_rec, training = biomass_tr)
#'
#' tidy(impute_rec, number = 1)
#'
#' transformed_te <- bake(impute_rec, biomass_te)
#'
#' plot(transformed_te$carbon, biomass_te$carbon,
#'      ylab = "pre-imputation", xlab = "imputed")


step_lowerimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           threshold = NULL,
           skip = FALSE,
           id = rand_id("lowerimpute")) {
    add_step(
      recipe,
      step_lowerimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        threshold = threshold,
        skip = skip,
        id = id
      )
    )
  }

step_lowerimpute_new <-
  function(terms, role, trained, threshold, skip, id) {
    step(
      subclass = "lowerimpute",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lowerimpute <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  threshold <-
    vapply(training[, col_names],
           min,
           numeric(1),
           na.rm = TRUE)
  if (any(threshold < 0))
    rlang::abort(
      paste0(
        "Some columns have negative values. Lower bound ",
        "imputation is intended for data bounded at zero."
      )
    )
  step_lowerimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = threshold,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lowerimpute <- function(object, new_data, ...) {
  for (i in names(object$threshold)) {
    affected <- which(new_data[[i]] <= object$threshold[[i]])
    if (length(affected) > 0)
      new_data[[i]][affected] <- runif(length(affected),
                                      max = object$threshold[[i]])
  }
  as_tibble(new_data)
}

print.step_lowerimpute <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Lower Bound Imputation for ", sep = "")
    printer(names(x$threshold), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_lowerimpute
#' @param x A `step_lowerimpute` object.
#' @export
tidy.step_lowerimpute <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$threshold),
                  value = unname(x$threshold))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl)
  }
  res$id <- x$id
  res
}
