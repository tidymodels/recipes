#' PCA Signal Extraction.
#' 
#' \code{step_pca} creates a \emph{specification} of a recipe step that will convert numeric data into one or more principal components. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[stats]{prcomp}}. Defaults are set for the arguments \code{center} and \code{scale.} but others can be passed in (e.g. \code{tol}). \bold{Note} that the argument \code{x} should not be passed here (or at all).
#' @param res The \code{\link[stats]{prcomp}} object is stored here once this preprocessing step has be trained by \code{\link{learn.recipe}}.
#' @param prefix A character string that will be the prefix to the resulting new variables. See notes below
#' @return \code{step_pca}  returns an object of class \code{step_pca}. 
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @export
#' @import dimRed

step_pca <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5, 
                     options = list(center = TRUE, scale. = TRUE, retx = FALSE),
                     res = NULL,
                     prefix = "PC") {
  add_step(
    recipe, 
    step_pca_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      res = res,
      prefix = prefix
    )
  )
}

step_pca_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL, 
                         options = NULL,
                         res = NULL,
                         prefix = "PC") {
  
  step(
    subclass = "pca",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    res = res,
    prefix = prefix
  )
}

learn.step_pca <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  x$num <- min(x$num, length(col_names))
  
  prc <- PCA(stdpars = x$options)
  
  prc <- prc@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                 list(ndim = x$num))
  
  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = prc,
    prefix = x$prefix
  )
}

process.step_pca <- function(object, newdata, ...) {
  pca_vars <- rownames(environment(object$res@apply)$rot)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, pca_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_pca <- function(x, form_width = 30, ...) {
  cat("PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
