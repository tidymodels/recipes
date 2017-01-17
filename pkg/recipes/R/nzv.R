#' Declare Which Variables Are Used Near-Zero Variance Filter.
#' 
#' This function is a \emph{specification} of a recipe step that will potentially remove variables that are highly sparse and unbalanced. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will evaluated by the filtering process.
#' @param role Not used by this step since no new variables are created.
#' @param options A list of options for \code{\link[caret]{nearZeroVar}}. \bold{Note} that the arguments \code{data} and \code{names} should not be included in this list. 
#' @param removals A character string that contains the names of columns that should be removed. These values are not determined until \code{\link{learn.nzv_step}}. 
#' @return An object of class \code{nzv_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' 

step_nzv <- function(recipe, 
                     terms, 
                     role = NA,
                     options = list(freqCut = 95 / 5, uniqueCut = 10),
                     removals = NULL) {
  add_step(
    recipe, 
    step_nzv_new(
      terms = terms, 
      role = role,
      options = options,
      removals = removals
    )
  )
}

step_nzv_new <- function(terms = NULL, 
                         role = NA,
                         options = NULL,
                         removals = NULL) {
  step(
    subclass = "nzv", 
    terms = terms,
    role = role,
    options = options,
    removals = removals
  )
}


#' Determine Which Variables to Remove Using a Training Set.
#' 
#' For a training set of data, this function uses \code{\link[caret]{nearZeroVar}} to determine which, if any, columns in the training set have sparse and unbalanced distributions.
#' #' 
#' @param x a \code{nzv_step} object that contains the list of predictors that should be removed.
#' @param data a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{nzv_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' @importFrom caret nearZeroVar
learn.nzv_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  data <- data[, col_names]
  filter <- do.call("nearZeroVar", c(list(x = data, names = TRUE), x$options))
  step_nzv_new(
    terms = x$terms, 
    role = x$role,
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
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' @importFrom tibble as_tibble
process.nzv_step <- function(x, data, ...) {
  if(length(x$removals) > 0)
    data <- data[, !(colnames(data) %in% x$removals)]
  as_tibble(data)
}

print.nzv_step <- function(x, form_width = 30, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$means)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

