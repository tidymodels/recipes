#' ICA Signal Extraction.
#' 
#' \code{step_ica} creates a \emph{specification} of a recipe step that will convert numeric data into one or more independent components. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new independent component columns created by the original variables will be used as predictors in a model. 
#' @param num The number of ICA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[fastICA]{fastICA}}. No defaults are set here. \bold{Note} that the arguments \code{X} and \code{n.comp} should not be passed here.
#' @param res The \code{\link[fastICA]{fastICA}} object is stored here once this preprocessing step has be trained by \code{\link{learn.step_ica}}.
#' @return \code{step_ica} and \code{learn.step_ica} return objects of class \code{step_ica}. 
#' @keywords datagen
#' @concept preprocessing ica projection_methods
#' @export

step_ica <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5, 
                     options = list(),
                     res = NULL) {
  add_step(
    recipe, 
    step_ica_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      res = res
    )
  )
}

step_ica_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL, 
                         options = NULL,
                         res = NULL) {
  
  step(
    subclass = "ica",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    res = res
  )
}

#' For a training set of data, \code{learn.step_ica} estimates the loadings for the independent components. This transformation only compute the required statistics for ICA. This function is \emph{not} intended to be directly called by the user. 
#'
#' @param x A \code{step_ica} object that contains the ICA specifications. 
#' @inheritParams learn.step_center
#' @param training A tibble or data frame that contains the training set.  These data will be used to compute the loadings that are used when this step is applied.
#' @importFrom dimRed FastICA dimRedData
#' @export
#' @rdname step_ica
learn.step_ica <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  x$num <- min(x$num, length(col_names))
  
  indc <- FastICA(stdpars = x$options)
  
  indc <- indc@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                   list(ndim = x$num))
  
  step_ica_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = indc
  )
}

#'  \code{process.step_ica} is used to compute the components on specific data sets. This creates new columns in the data set and removes the original columns. This function is \emph{not} intended to be directly called by the user. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be converted to independent components.
#' @return \code{process.step_ica} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom dimRed dimRedData
#' @export
#' @rdname step_ica
process.step_ica <- function(object, newdata, ...) {
  ica_vars <- colnames(environment(object$res@apply)$indata)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, ica_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), "IC")
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% ica_vars), drop = FALSE]
  as_tibble(newdata)
}

#' @export
print.step_ica <- function(x, form_width = 30, ...) {
  cat("ICA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
