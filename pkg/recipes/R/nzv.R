#' Declare Which Variables Are Used Near-Zero Variance Filter.
#' 
#' This function is a \emph{specification} of a recipe step that will potentially remove variables that are highly sparse and unbalanced. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will evaluated by the filtering process.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param options A list of options for \code{\link[caret]{nearZeroVar}}. \bold{Note} that the arguments \code{data} and \code{names} should not be included in this list. 
#' @param removals A character string that contains the names of columns that should be removed. These values are not determined until \code{\link{learn.nzv_step}}. 
#' @return An object of class \code{nzv_step}. 
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


#' Determine Which Variables to Remove Using a Training Set.
#' 
#' For a training set of data, this function uses \code{\link[caret]{nearZeroVar}} to determine which, if any, columns in the training set have sparse and unbalanced distributions.
#'
#' @param x a \code{nzv_step} object that contains the list of predictors that should be removed.
#' @param data a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{nzv_step}. 
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' @importFrom caret nearZeroVar
#' @rdname step_nzv

learn.nzv_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  nzv_call <- quote(nearZeroVar(x, freqCut, uniqueCut, saveMetrics, names = TRUE, foreach, allowParallel))
  args <- sub_args(nearZeroVar, x$options)
  args$x <- data[, col_names]
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

#' Filter Near-Zero Variance Columns form a Data Set.
#' 
#' For a trained \code{nzv_step} object, this function potentially removes columns from the data. 
#' 
#' @param x A trained \code{nzv_step} object.
#' @param data A tibble or data frame.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_nzv

process.nzv_step <- function(x, data, ...) {
  if(length(x$removals) > 0)
    data <- data[, !(colnames(data) %in% x$removals)]
  as_tibble(data)
}

#' @export
print.nzv_step <- function(x, form_width = 30, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

