#' Declare Which Variables Are Used for PCA Signal Extraction.
#' 
#' This function is a \emph{specification} of a recipe step that will convert numeric data into one or more principal components. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[stats]{prcomp}}. Defaults are set for the arguments \code{center} and \code{scale.} but others can be passed in (e.g. \code{tol}). \bold{Note} that the arguments \code{x} and \code{y} should not be passed here (or at all).
#' @param object The \code{\link[stats]{prcomp}} object is stored here once this preprocessing step has be trained by \code{\link{learn.pca_step}}.
#' @return An object of class \code{pca_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @export

step_pca <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5, 
                     options = list(center = TRUE, scale. = TRUE),
                     object = NULL) {
  add_step(
    recipe, 
    step_pca_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      object = object
    )
  )
}

step_pca_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL, 
                         options = NULL,
                         object = NULL) {
  
  step(
    subclass = "pca",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    object = object
  )
}

#' Estimate the Principal Component Loadings from a Training Set.
#' 
#' For a training set of data, this function uses \code{\link[stats]{prcomp}} to estimate the loadings for the principal components. This transformation only compute the required statistics for PCA while \code{\link{process}} is used to compute the components on specific data sets. 
#'
#' @param x a \code{pca_step} object that contains the PCA objects.
#' @param data a tibble or data frame that contains the training set. These data will be used to compute the loadings that are used when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{pca_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @importFrom stats as.formula model.frame prcomp
learn.pca_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  dat <- data[, col_names]
  prc <- do.call("prcomp", c(list(x = dat), x$options))
  
  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = min(x$num, ncol(dat)),
    options = x$options,
    object = prc
  )
}


#' Compute the Principal Components for a Data Set.
#' 
#' For a trained \code{pca_step} object, this function projects the current data into the principal components defined by the training set. This creates new columns in the data set and removes the original columns. 
#' 
#' @param x A trained \code{pca_step} object.
#' @param data A tibble or data frame that has numeric variables that will be converted to principal components.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @importFrom tibble as_tibble
#' @importFrom stats predict
process.pca_step <- function(x, data, ...) {
  pca_vars <- rownames(x$object$rotation)
  comps <- predict(x$object, data[, pca_vars, drop = FALSE])
  comps <- comps[, 1:x$num, drop = FALSE]
  data <- cbind(data, comps)
  data <- data[, !(colnames(data) %in% pca_vars), drop = FALSE]
  as_tibble(data)
}


print.pca_step <- function(x, form_width = 30, ...) {
  cat("PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
