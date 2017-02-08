#' Near-Zero Variance Filter.
#' 
#' \code{step_nzv} creates a \emph{specification} of a recipe step that will potentially remove variables that are highly sparse and unbalanced. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will evaluated by the filtering process.
#' @param role Not used by this step since no new variables are created.
#' @param options A list of options for \code{\link[caret]{nearZeroVar}}. \bold{Note} that the arguments \code{data} and \code{names} should not be included in this list. 
#' @param removals A character string that contains the names of columns that should be removed. These values are not determined until \code{\link{learn.recipe}} is called. 
#' @return \code{step_nzv}  returns an object of class \code{step_nzv}.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' 

step_nzv <- function(recipe, 
                     terms, 
                     role = NA,
                     trained = FALSE,
                     options = list(freqCut = 95 / 5, uniqueCut = 10),
                     removals = NULL) {
  add_step(
    recipe, 
    step_nzv_new(
      terms = terms, 
      role = role,
      trained = trained,
      options = options,
      removals = removals
    )
  )
}

step_nzv_new <- function(terms = NULL, 
                         role = NA,
                         trained = FALSE,
                         options = NULL,
                         removals = NULL) {
  step(
    subclass = "nzv", 
    terms = terms,
    role = role,
    trained = trained,
    options = options,
    removals = removals
  )
}

learn.step_nzv <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  filter <- nzv(x = training[, col_names], 
                freqCut = x$options$freqCut, 
                uniqueCut = x$options$uniqueCut)
  
  step_nzv_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE,
    options = x$options,
    removals = filter
  )
}

process.step_nzv <- function(object, newdata, ...) {
  if(length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_nzv <- function(x, form_width = 30, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}



nzv <- function (x, freqCut = 95/5, uniqueCut = 10){
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  
  fr_foo <- function(data){
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0);
    }
    w <- which.max(t);
    return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
  }
  
  freqRatio <- vapply(x, fr_foo, c(ratio = 0))
  uni_foo <- function(data) length(unique(data[!is.na(data)]))
  lunique <- vapply(x, uni_foo, c(num = 0))
  percentUnique <- 100 * lunique / vapply(x, length, c(num = 0))
  
  zero_func <- function(data) all(is.na(data))
  zeroVar <- (lunique == 1) | vapply(x, zero_func, c(zv = TRUE))
  
  out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  names(out) <- NULL
  colnames(x)[out]
}

