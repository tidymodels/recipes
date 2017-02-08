#' Kernel PCA Signal Extraction.
#' 
#' \code{step_kpca} a \emph{specification} of a recipe step that will convert numeric data into one or more principal components using a kernel basis expansion. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[kernlab]{kpca}}. Defaults are set for the arguments \code{kernel} and \code{kpar} but others can be passed in. \bold{Note} that the arguments \code{x} and \code{features} should not be passed here (or at all).
#' @param res An S4 \code{\link[kernlab]{kpca}} object is stored here once this preprocessing step has be trained by \code{\link{learn.recipe}}.
#' @param prefix A character string that will be the prefix to the resulting new variables. See notes below
#' @return \code{step_kpca} returns an object of class \code{step_kpca}. 
#' @keywords datagen
#' @concept preprocessing pca projection_methods kernel_methods
#' @export
#' @import dimRed

step_kpca <- function(recipe, 
                      terms, 
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
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      res = res,
      options = options,
      prefix = prefix
    )
  )
}

step_kpca_new <- function(terms = NULL, 
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

learn.step_kpca <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  kprc <- kPCA(stdpars = c(list(ndim = x$num), x$options))
  kprc <- kprc@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
                   kprc@stdpars)
  
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

process.step_kpca <- function(object, newdata, ...) {
  pca_vars <- colnames(environment(object$res@apply)$indata)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, pca_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_kpca <- function(x, form_width = 30, ...) {
  cat("Kernel PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
