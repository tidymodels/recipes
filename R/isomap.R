#' Isomap Embedding.
#' 
#' \code{step_isomap} creates a \emph{specification} of a recipe step that will convert numeric data into one or more new dimensions. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the dimensions.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new dimension columns created by the original variables will be used as predictors in a model. 
#' @param num The number of isomap dimensions to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible dimensions, a smaller value will be used. 
#' @param options A list of options to \code{\link[dimRed]{Isomap}}. 
#' @param res The \code{\link[dimRed]{Isomap}} object is stored here once this preprocessing step has be trained by \code{\link{learn.step_isomap}}.
#' @return \code{step_isomap} and \code{learn.step_isomap} return objects of class \code{step_isomap}. 
#' @keywords datagen
#' @concept preprocessing isomap projection_methods
#' @export

step_isomap <- function(recipe, 
                        terms, 
                        role = "predictor",
                        trained = FALSE,
                        num  = 5, 
                        options = list(knn = 50, .mute = c("message", "output")),
                        res = NULL) {
  add_step(
    recipe, 
    step_isomap_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      res = res
    )
  )
}

step_isomap_new <- function(terms = NULL, 
                            role = "predictor",
                            trained = FALSE,
                            num  = NULL, 
                            options = NULL,
                            res = NULL) {
  
  step(
    subclass = "isomap",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    res = res
  )
}

#' For a training set of data, \code{learn.step_isomap} estimates the mappings for the new dimensions. This transformation only computes the required statistics for the Isomap approximation. This function is \emph{not} intended to be directly called by the user. 
#'
#' @param x A \code{step_isomap} object that contains the Isomap specifications. 
#' @param training A tibble or data frame that contains the training set. These data will be used to compute the mappings that are used when this step is applied.
#' @importFrom dimRed Isomap dimRedData embed dimRedMethodList
#' @export
#' @rdname step_isomap
learn.step_isomap <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  x$num <- min(x$num, ncol(training))
  x$options$knn <- min(x$options$knn, nrow(training))
  
  imap <- embed(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                "Isomap", knn = x$options$knn, ndim = x$num, .mute = x$options$.mute)
  
  step_isomap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = imap
  )
}

#'  \code{process.step_isomap} is used to compute the new dimensions on specific data sets. This creates new columns in the data set and removes the original columns. This function is \emph{not} intended to be directly called by the user. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be converted to new dimensions.
#' @return \code{process.step_isomap} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom dimRed dimRedData
#' @export
#' @rdname step_isomap
process.step_isomap <- function(object, newdata, ...) {
  isomap_vars <- colnames(environment(object$res@apply)$indata)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, isomap_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), "Isomap")
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% isomap_vars), drop = FALSE]
  as_tibble(newdata)
}

#' @export
print.step_isomap <- function(x, form_width = 30, ...) {
  cat("Isomap approximation with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
