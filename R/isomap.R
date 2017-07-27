#' Isomap Embedding
#'
#' \code{step_isomap} creates a \emph{specification} of a recipe step that will
#'   convert numeric data into one or more new dimensions.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'   used to compute the dimensions. See \code{\link{selections}} for more
#'   details.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the new
#'   dimension columns created by the original variables will be used as
#'   predictors in a model.
#' @param num The number of isomap dimensions to retain as new predictors. If
#'   \code{num} is greater than the number of columns or the number of
#'   possible dimensions, a smaller value will be used.
#' @param options A list of options to \code{\link[dimRed]{Isomap}}.
#' @param res The \code{\link[dimRed]{Isomap}} object is stored here once this
#'   preprocessing step has be trained by \code{\link{prep.recipe}}.
#' @param prefix A character string that will be the prefix to the resulting
#'   new variables. See notes below
#' @keywords datagen
#' @concept preprocessing isomap projection_methods
#' @export
#' @details Isomap is a form of multidimensional scaling (MDS). MDS methods
#'   try to find a reduced set of dimensions such that the geometric distances
#'   between the original data points are preserved. This version of MDS uses
#'   nearest neighbors in the data as a method for increasing the fidelity of
#'   the new dimensions to the original data values.
#'
#' It is advisable to center and scale the variables prior to running Isomap
#'   (\code{step_center} and \code{step_scale} can be used for this purpose).
#'
#' The argument \code{num} controls the number of components that will be
#'   retained (the original variables that are used to derive the components
#'   are removed from the data). The new components will have names that begin
#'   with \code{prefix} and a sequence of numbers. The variable names are
#'   padded with zeros. For example, if \code{num < 10}, their names will be
#'   \code{Isomap1} - \code{Isomap9}. If \code{num = 101}, the names would be
#'   \code{Isomap001} - \code{Isomap101}.
#' @references De Silva, V., and Tenenbaum, J. B. (2003). Global versus local
#'   methods in nonlinear dimensionality reduction. \emph{Advances in Neural
#'   Information Processing Systems}. 721-728.
#'
#' \pkg{dimRed}, a framework for dimensionality reduction,
#'   \url{https://github.com/gdkrmr}
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
#' @seealso \code{\link{step_pca}} \code{\link{step_kpca}}
#'   \code{\link{step_ica}} \code{\link{recipe}} \code{\link{prep.recipe}}
#'   \code{\link{bake.recipe}}

step_isomap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num  = 5,
           options = list(knn = 50, .mute = c("message", "output")),
           res = NULL,
           prefix = "Isomap") {
    add_step(
      recipe,
      step_isomap_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        num = num,
        options = options,
        res = res,
        prefix = prefix
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
           prefix = "isomap") {
    step(
      subclass = "isomap",
      terms = terms,
      role = role,
      trained = trained,
      num = num,
      options = options,
      res = res,
      prefix = prefix
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
    prefix = x$prefix
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
