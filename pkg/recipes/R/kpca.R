#' Kernel PCA Signal Extraction.
#' 
#' \code{step_kpca} a \emph{specification} of a recipe step that will convert numeric data into one or more principal components using a kernel basis expansion. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[kernlab]{kpca}}. Defaults are set for the arguments \code{kernel} and \code{kpar} but others can be passed in. \bold{Note} that the arguments \code{x} and \code{features} should not be passed here (or at all).
#' @param object An S4 \code{\link[kernlab]{kpca}} object is stored here once this preprocessing step has be trained by \code{\link{learn.kpca_step}}.
#' @return \code{step_kpca} and \code{learn.kpca_step} return objects of class \code{kpca_step}. 
#' @keywords datagen
#' @concept preprocessing pca projection_methods kernel_methods
#' @export

step_kpca <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5,
                     object = NULL,
                     options = list(kernel = "rbfdot", 
                                    kpar = list(sigma = 0.2))) {
  add_step(
    recipe, 
    step_kpca_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      object = object,
      options = options
    )
  )
}

step_kpca_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL,
                         object = NULL,
                         options = NULL) {
  
  step(
    subclass = "kpca",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    object = object,
    options = options
  )
}

#' For a training set of data, \code{learn.kpca_step} uses \code{\link[kernlab]{kpca}} to estimate the loadings for the principal components in the kernel space. This transformation only compute the required statistics for kernel PCA. 
#'
#' @param x A \code{kpca_step} object that contains the kernel PCA specifications. 
#' @param data a tibble or data frame that contains the training set. These data will be used to compute the loadings that are used when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @importFrom dimRed kPCA dimRedData
#' @rdname step_kpca
learn.kpca_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  kprc <- kPCA(stdpars = c(list(ndim = x$num), x$options))
  kprc <- kprc@fun(dimRedData(as.data.frame(data[, col_names, drop = FALSE])),
                   kprc@stdpars)
  
  step_kpca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    object = kprc
  )
}

#'  \code{process.kpca_step} is used to compute the components on specific data sets. This creates new columns in the data set and removes the original columns. 
#' 
#' @param data A tibble or data frame that has numeric variables that will be converted to principal components.
#' @return \code{process.kpca_step} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom dimRed dimRedData
#' @rdname step_kpca
process.kpca_step <- function(x, data, ...) {
  pca_vars <- filter_terms(x$terms, data) 
  comps <- x$object@apply(dimRedData(as.data.frame(data[, pca_vars, drop = FALSE])))@data
  comps <- comps[, 1:x$num, drop = FALSE]
  data <- cbind(data, comps)
  data <- data[, !(colnames(data) %in% pca_vars), drop = FALSE]
  as_tibble(data)
}

#' @export
print.kpca_step <- function(x, form_width = 30, ...) {
  cat("Kernel PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
