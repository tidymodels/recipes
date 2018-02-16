#' Isomap Embedding
#'
#' `step_isomap` creates a *specification* of a recipe
#'  step that will convert numeric data into one or more new
#'  dimensions.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the dimensions. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new dimension columns created by the original variables
#'  will be used as predictors in a model.
#' @param num The number of isomap dimensions to retain as new
#'  predictors. If `num` is greater than the number of columns
#'  or the number of possible dimensions, a smaller value will be
#'  used.
#' @param options A list of options to
#'  [dimRed::Isomap()].
#' @param res The [dimRed::Isomap()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @keywords datagen
#' @concept preprocessing isomap projection_methods
#' @export
#' @details Isomap is a form of multidimensional scaling (MDS).
#'  MDS methods try to find a reduced set of dimensions such that
#'  the geometric distances between the original data points are
#'  preserved. This version of MDS uses nearest neighbors in the
#'  data as a method for increasing the fidelity of the new
#'  dimensions to the original data values.
#'
#' It is advisable to center and scale the variables prior to
#'  running Isomap (`step_center` and `step_scale` can be
#'  used for this purpose).
#'
#' The argument `num` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num < 10`, their names will be `Isomap1` -
#'  `Isomap9`. If `num = 101`, the names would be
#'  `Isomap001` - `Isomap101`.
#' @references De Silva, V., and Tenenbaum, J. B. (2003). Global
#'  versus local methods in nonlinear dimensionality reduction.
#'  *Advances in Neural Information Processing Systems*.
#'  721-728.
#'
#' \pkg{dimRed}, a framework for dimensionality reduction,
#'   https://github.com/gdkrmr
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' im_trans <- rec %>%
#'   step_YeoJohnson(all_predictors()) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_isomap(all_predictors(),
#'               options = list(knn = 100),
#'               num = 2)
#'
#' im_estimates <- prep(im_trans, training = biomass_tr)
#'
#' im_te <- bake(im_estimates, biomass_te)
#'
#' rng <- extendrange(c(im_te$Isomap1, im_te$Isomap2))
#' plot(im_te$Isomap1, im_te$Isomap2,
#'      xlim = rng, ylim = rng)
#'
#' tidy(im_trans, number = 4)
#' tidy(im_estimates, number = 4)
#' @seealso [step_pca()] [step_kpca()]
#'   [step_ica()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_isomap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num  = 5,
           options = list(knn = 50, .mute = c("message", "output")),
           res = NULL,
           prefix = "Isomap",
           skip = FALSE) {
    add_step(
      recipe,
      step_isomap_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num = num,
        options = options,
        res = res,
        prefix = prefix,
        skip = skip
      )
    )
  }

step_isomap_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           num  = NULL,
           options = NULL,
           res = NULL,
           prefix = "isomap",
           skip = FALSE) {
    step(
      subclass = "isomap",
      terms = terms,
      role = role,
      trained = trained,
      num = num,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip
    )
  }

#' @importFrom dimRed embed dimRedData
#' @export
prep.step_isomap <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  x$num <- min(x$num, ncol(training))
  x$options$knn <- min(x$options$knn, nrow(training))

  imap <-
    embed(
      dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
      "Isomap",
      knn = x$options$knn,
      ndim = x$num,
      .mute = x$options$.mute
    )

  step_isomap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = imap,
    prefix = x$prefix,
    skip = x$skip
  )
}

#' @export
bake.step_isomap <- function(object, newdata, ...) {
  isomap_vars <- colnames(environment(object$res@apply)$indata)
  comps <-
    object$res@apply(
      dimRedData(as.data.frame(newdata[, isomap_vars, drop = FALSE]))
      )@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- cbind(newdata, as_tibble(comps))
  newdata <-
    newdata[, !(colnames(newdata) %in% isomap_vars), drop = FALSE]
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}


print.step_isomap <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Isomap approximation with ")
    printer(colnames(x$res@org.data), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_isomap
#' @param x A `step_isomap` object
tidy.step_isomap <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = colnames(x$res@org.data))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res
}
