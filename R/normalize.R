#' Center and scale numeric data
#'
#' `step_normalize` creates a *specification* of a recipe
#'  step that will normalize numeric data to have a standard
#'  deviation of one and a mean of zero.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param means A named numeric vector of means. This is
#'  `NULL` until computed by [prep.recipe()].
#' @param sds A named numeric vector of standard deviations This
#'  is `NULL` until computed by [prep.recipe()].
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed when computing the standard deviation and mean.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the
#'  standard deviations and means), and `statistic` for the type of value.
#' @keywords datagen
#' @concept preprocessing
#' @concept normalization_methods
#' @export
#' @details Centering data means that the average of a variable is subtracted
#'  from the data. Scaling data means that the standard deviation of a variable
#'  is divided out of the data. `step_normalize` estimates the variable standard
#'  deviations and means from the data used in the `training` argument of
#'  `prep.recipe`. `bake.recipe` then applies the scaling to new data sets using
#'  these estimates.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' norm_trans <- rec %>%
#'   step_normalize(carbon, hydrogen)
#'
#' norm_obj <- prep(norm_trans, training = biomass_tr)
#'
#' transformed_te <- bake(norm_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' tidy(norm_trans, number = 1)
#' tidy(norm_obj, number = 1)
#'
step_normalize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           sds = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("normalize")) {
    add_step(
      recipe,
      step_normalize_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        means = means,
        sds = sds,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_normalize_new <-
  function(terms, role, trained, means, sds, na_rm, skip, id) {
    step(
      subclass = "normalize",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      sds = sds,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @importFrom stats sd
#' @export
prep.step_normalize <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na_rm)
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na_rm)
  step_normalize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    sds = sds,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_normalize <- function(object, new_data, ...) {
  res <- sweep(as.matrix(new_data[, names(object$means)]), 2, object$means, "-")
  res <- sweep(res, 2, object$sds, "/")
  if (is.matrix(res) && ncol(res) == 1)
    res <- res[, 1]
  new_data[, names(object$sds)] <- res
  as_tibble(new_data)
}

print.step_normalize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Centering and scaling for ", sep = "")
    printer(names(x$sds), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_normalize
#' @param x A `step_normalize` object.
#' @export
tidy.step_normalize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = c(names(x$means), names(x$sds)),
                  statistic = rep(c("mean", "sd"), each = length(x$sds)),
                  value = c(x$means, x$sds))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  statistic = na_chr,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}
