#' Isomap Embedding.
#' 
#' \code{step_isomap} creates a \emph{specification} of a recipe step that will convert numeric data into one or more new dimensions. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the dimensions.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new dimension columns created by the original variables will be used as predictors in a model. 
#' @param num The number of isomap dimensions to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible dimensions, a smaller value will be used. 
#' @param options A list of options to \code{\link[dimRed]{Isomap}}. 
#' @param res The \code{\link[dimRed]{Isomap}} object is stored here once this preprocessing step has be trained by \code{\link{learn.recipe}}.
#' @return \code{step_isomap} returns an object of class \code{step_isomap}. 
#' @keywords datagen
#' @concept preprocessing isomap projection_methods
#' @export
#' @import dimRed

step_isomap <- function(recipe, 
                        terms, 
                        role = "predictor",
                        trained = FALSE,
                        num  = 5, 
                        options = list(knn = 50),
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

learn.step_isomap <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  x$num <- min(x$num, ncol(training))
  x$options$knn <- min(x$options$knn, nrow(training))
  
  imap <- Isomap(stdpars = x$options)
  imap <- imap@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                 list(ndim = x$num, knn = x$options$knn))
  step_isomap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = imap
  )
}

process.step_isomap <- function(object, newdata, ...) {
  isomap_vars <- colnames(environment(object$res@apply)$indata)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, isomap_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), "Isomap")
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% isomap_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_isomap <- function(x, form_width = 30, ...) {
  cat("Isomap approximation with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
