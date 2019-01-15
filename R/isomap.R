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
#' @param num_terms The number of isomap dimensions to retain as new
#'  predictors. If `num_terms` is greater than the number of columns
#'  or the number of possible dimensions, a smaller value will be
#'  used.
#' @param neighbors The number of neighbors.
#' @param options A list of options to [dimRed::Isomap()].
#' @param num The number of isomap dimensions (this will be deprecated
#'  in factor of  `num_terms` in version 0.1.5). `num_terms` will
#'  override this option.
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
#' This step requires the \pkg{dimRed}, \pkg{RSpectra},
#'  \pkg{igraph}, and \pkg{RANN} packages. If not installed, the
#'  step will stop with a note about installing these packages.
#'
#'
#' It is advisable to center and scale the variables prior to
#'  running Isomap (`step_center` and `step_scale` can be
#'  used for this purpose).
#'
#' The argument `num_terms` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_terms < 10`, their names will be `Isomap1` -
#'  `Isomap9`. If `num_terms = 101`, the names would be
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
#' \donttest{
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
#'               neighbors = 100,
#'               num_terms = 2)
#'
#' if (require(dimRed) & require(RSpectra)) {
#'   im_estimates <- prep(im_trans, training = biomass_tr)
#'
#'   im_te <- bake(im_estimates, biomass_te)
#'
#'   rng <- extendrange(c(im_te$Isomap1, im_te$Isomap2))
#'   plot(im_te$Isomap1, im_te$Isomap2,
#'        xlim = rng, ylim = rng)
#'
#'   tidy(im_trans, number = 4)
#'   tidy(im_estimates, number = 4)
#' }
#' }
#' @seealso [step_pca()] [step_kpca()]
#'   [step_ica()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_isomap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_terms  = 5,
           neighbors = 50,
           options = list(.mute = c("message", "output")),
           res = NULL,
           num = NULL,
           prefix = "Isomap",
           skip = FALSE,
           id = rand_id("isomap")) {

    recipes_pkg_check(c("dimRed", "RSpectra", "igraph", "RANN"))
    if (!is.null(num))
      message("The argument `num` is deprecated in factor of `num_terms`. ",
              "`num` will be removed in next version.", call. = FALSE)
    add_step(
      recipe,
      step_isomap_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_terms = num_terms,
        neighbors = neighbors,
        options = options,
        res = res,
        num = num,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_isomap_new <-
  function(terms, role, trained, num_terms, neighbors, options, res, num,
           prefix, skip, id) {
    step(
      subclass = "isomap",
      terms = terms,
      role = role,
      trained = trained,
      num_terms = num_terms,
      neighbors = neighbors,
      options = options,
      res = res,
      num = num,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_isomap <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  x$num_terms <- min(x$num_terms, ncol(training))
  x$neighbors <- min(x$neighbors, nrow(training))

  imap <-
    dimRed::embed(
      dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
      "Isomap",
      knn = x$neighbors,
      ndim = x$num_terms,
      .mute = x$options$.mute
    )

  step_isomap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_terms = x$num_terms,
    neighbors = x$neighbors,
    options = x$options,
    res = imap,
    num = x$num_terms,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_isomap <- function(object, new_data, ...) {
  isomap_vars <- colnames(environment(object$res@apply)$indata)
  comps <-
    object$res@apply(
      dimRed::dimRedData(as.data.frame(new_data[, isomap_vars, drop = FALSE]))
      )@data
  comps <- comps[, 1:object$num_terms, drop = FALSE]
  comps <- check_name(comps, new_data, object)
  new_data <- bind_cols(new_data, as_tibble(comps))
  new_data <-
    new_data[, !(colnames(new_data) %in% isomap_vars), drop = FALSE]
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}


print.step_isomap <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Isomap approximation with ")
    printer(colnames(x$res@org.data), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_isomap
#' @param x A `step_isomap` object
#' @export
tidy.step_isomap <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = colnames(x$res@org.data))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
