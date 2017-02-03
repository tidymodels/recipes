#' PCA Signal Extraction.
#' 
#' \code{step_pca} creates a \emph{specification} of a recipe step that will convert numeric data into one or more principal components. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[stats]{prcomp}}. Defaults are set for the arguments \code{center} and \code{scale.} but others can be passed in (e.g. \code{tol}). \bold{Note} that the arguments \code{x} and \code{y} should not be passed here (or at all).
#' @param res The \code{\link[stats]{prcomp}} object is stored here once this preprocessing step has be trained by \code{\link{learn.step_pca}}.
#' @return \code{step_pca} and \code{learn.step_pca} return objects of class \code{step_pca}. 
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @export

step_pca <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5, 
                     options = list(center = TRUE, scale. = TRUE, retx = FALSE),
                     res = NULL) {
  add_step(
    recipe, 
    step_pca_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      res = res
    )
  )
}

step_pca_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL, 
                         options = NULL,
                         res = NULL) {
  
  step(
    subclass = "pca",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    res = res
  )
}

#' For a training set of data, \code{learn.step_pca} estimates the loadings for the principal components. This transformation only compute the required statistics for PCA. 
#'
#' @param x A \code{step_pca} object that contains the PCA specifications. 
#' @param training A tibble or data frame that contains the training set. These data will be used to compute the loadings that are used when this step is applied.
#' @importFrom dimRed PCA dimRedData
#' @export
#' @rdname step_pca
learn.step_pca <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  
  x$num <- min(x$num, ncol(training))
  
  prc <- PCA(stdpars = x$options)
  
  prc <- prc@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                 list(ndim = x$num))
  
  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = prc
  )
}

#'  \code{process.step_pca} is used to compute the components on specific data sets. This creates new columns in the data set and removes the original columns. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be converted to principal components.
#' @return \code{process.step_pca} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom dimRed dimRedData
#' @export
#' @rdname step_pca
process.step_pca <- function(object, newdata, ...) {
  pca_vars <- filter_terms(object$terms, newdata) 
  # comps <- predict(object$object, dimRedData(data[, pca_vars, drop = FALSE]))@data
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, pca_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), "PC")
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

#' @export
print.step_pca <- function(x, form_width = 30, ...) {
  cat("PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
