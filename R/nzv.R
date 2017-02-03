#' Near-Zero Variance Filter.
#' 
#' \code{step_nzv} creates a \emph{specification} of a recipe step that will potentially remove variables that are highly sparse and unbalanced. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will evaluated by the filtering process.
#' @param role Not used by this step since no new variables are created.
#' @param options A list of options for \code{\link[caret]{nearZeroVar}}. \bold{Note} that the arguments \code{data} and \code{names} should not be included in this list. 
#' @param removals A character string that contains the names of columns that should be removed. These values are not determined until \code{\link{learn.step_nzv}}. 
#' @return \code{step_nzv} and \code{learn.step_nzv} return objects of class \code{step_nzv}.
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


#' For a training set of data, \code{learn.step_nzv} determines which, if any, columns in the training set have sparse and unbalanced distributions.
#'
#' @param x a \code{step_nzv} object that contains the list of predictors that should be removed.
#' @inheritParams learn.step_center
#' @export
#' @importFrom caret nearZeroVar
#' @rdname step_nzv

learn.step_nzv <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  
  nzv_call <- quote(nearZeroVar(x, freqCut, uniqueCut, saveMetrics, names = TRUE, foreach, allowParallel))
  args <- sub_args(nearZeroVar, x$options)
  args$x <- training[, col_names]
  args$names <- TRUE
  filter <- eval(nzv_call, envir = args)
  
  step_nzv_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE,
    options = x$options,
    removals = filter
  )
}

#' \code{process.step_nzv} is used to potentially remove columns from the data. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame to be filtered.
#' @return \code{process.step_nzv} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_nzv

process.step_nzv <- function(object, newdata, ...) {
  if(length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

#' @export
print.step_nzv <- function(x, form_width = 30, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

