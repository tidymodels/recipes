#' Kernel PCA Signal Extraction
#'
#' \code{step_kpca} a \emph{specification} of a recipe step that will convert
#'   numeric data into one or more principal components using a kernel basis
#'   expansion.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'   used to compute the components. See \code{\link{selections}} for more
#'   details.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the new principal
#'   component columns created by the original variables will be used as
#'   predictors in a model.
#' @param num The number of PCA components to retain as new predictors. If
#'   \code{num} is greater than the number of columns or the number of possible
#'   components, a smaller value will be used.
#' @param options A list of options to \code{\link[kernlab]{kpca}}. Defaults
#'   are set for the arguments \code{kernel} and \code{kpar} but others can be
#'   passed in. \bold{Note} that the arguments \code{x} and \code{features}
#'   should not be passed here (or at all).
#' @param res An S4 \code{\link[kernlab]{kpca}} object is stored here once this
#'   preprocessing step has be trained by \code{\link{prep.recipe}}.
#' @param prefix A character string that will be the prefix to the resulting
#'   new variables. See notes below.
#' @keywords datagen
#' @concept preprocessing pca projection_methods kernel_methods
#' @export
#' @details Kernel principal component analysis (kPCA) is an extension a PCA
#'   analysis that conducts the calculations in a broader dimensionality
#'   defined by a kernel function. For example, if a quadratic kernel function
#'   were used, each variable would be represented by its original values as
#'   well as its square. This nonlinear mapping is used  during the PCA
#'   analysis and can potentially help find better representations of the
#'   original data.
#'
#' As with ordinary PCA, it is important to standardized the variables prior
#'   to running PCA (\code{step_center} and \code{step_scale} can be used for
#'   this purpose).
#'
#' When performing kPCA, the kernel function (and any important kernel
#'   parameters) must be chosen. The \pkg{kernlab} package is used and the
#'   reference below discusses the types of kernels available and their
#'   parameter(s). These specifications can be made in the \code{kernel} and
#'   \code{kpar} slots of the \code{options} argument to \code{step_kpca}.
#'
#' The argument \code{num} controls the number of components that will be
#'   retained (the original variables that are used to derive the components
#'   are removed from the data). The new components will have names that begin
#'   with \code{prefix} and a sequence of numbers. The variable names are
#'   padded with zeros. For example, if \code{num < 10}, their names will be
#'   \code{kPC1} - \code{kPC9}. If \code{num = 101}, the names would be
#'   \code{kPC001} - \code{kPC101}.
#'
#' @references Scholkopf, B., Smola, A., and Muller, K. (1997). Kernel
#'   principal component analysis. \emph{Lecture Notes in Computer Science},
#'   1327, 583-588.
#'
#' Karatzoglou, K., Smola, A., Hornik, K., and Zeileis, A. (2004). kernlab -
#'   An S4 package for kernel methods in R. \emph{Journal of Statistical
#'   Software}, 11(1), 1-20.
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
#' kpca_trans <- rec %>%
#'   step_YeoJohnson(all_predictors()) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_kpca(all_predictors())
#'
#' kpca_estimates <- prep(kpca_trans, training = biomass_tr)
#'
#' kpca_te <- bake(kpca_estimates, biomass_te)
#'
#' rng <- extendrange(c(kpca_te$kPC1, kpca_te$kPC2))
#' plot(kpca_te$kPC1, kpca_te$kPC2,
#'      xlim = rng, ylim = rng)
#' @seealso \code{\link{step_pca}} \code{\link{step_ica}}
#'   \code{\link{step_isomap}} \code{\link{recipe}} \code{\link{prep.recipe}}
#'   \code{\link{bake.recipe}}
#'
step_kpca <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num  = 5,
           res = NULL,
           options = list(kernel = "rbfdot",
                          kpar = list(sigma = 0.2)),
           prefix = "kPC") {
  add_step(
    recipe,
    step_kpca_new(
      terms = check_ellipses(...),
      role = role,
      trained = trained,
      num = num,
      res = res,
      options = options,
      prefix = prefix
    )
  )
}

step_kpca_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           num  = NULL,
           res = NULL,
           options = NULL,
           prefix = "kPC") {
  step(
    subclass = "kpca",
    terms = terms,
    role = role,
    trained = trained,
    num = num,
    res = res,
    options = options,
    prefix = prefix
  )
}

#' @importFrom dimRed kPCA dimRedData
#' @export
prep.step_kpca <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  kprc <- kPCA(stdpars = c(list(ndim = x$num), x$options))
  kprc <- kprc@fun(
    dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
    kprc@stdpars
  )

  step_kpca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = kprc,
    prefix = x$prefix
  )
}

#' @export
bake.step_kpca <- function(object, newdata, ...) {
  pca_vars <- colnames(environment(object$res@apply)$indata)
  comps <- object$res@apply(
    dimRedData(as.data.frame(newdata[, pca_vars, drop = FALSE]))
    )@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- cbind(newdata, as_tibble(comps))
  newdata <- newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_kpca <- function(x, width = max(20, options()$width - 40), ...) {
  if(x$trained) {
    cat("Kernel PCA (", x$res@pars$kernel, ") extraction with ", sep = "")
    cat(format_ch_vec(colnames(x$res@org.data), width = width))
  } else {
    cat("Kernel PCA extraction with ", sep = "")
    cat(format_selectors(x$terms, wdth = width))
  }
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
