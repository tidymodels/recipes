#' Declare Variables for Centering.
#' 
#' This function is a \emph{specification} of a recipe step that will normalize numeric data to have a mean of zero. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be centered.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. 
#' @param means A named numeric vector of means. This is \code{NULL} until computed by \code{\link{learn.center_step}}. 
#' @return An object of class \code{center_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export

step_center <- function(recipe, terms, role = NA, trained = FALSE, means = NULL) {
  add_step(
    recipe, 
    step_center_new(
      terms = terms, 
      trained = trained,
      role = role, 
      means = means))
}

## Initializes a new object
step_center_new <- function(terms = NULL, role = NA, trained = FALSE, means = NULL) {
  step(
    subclass = "center", 
    terms = terms,
    role = role,
    trained = trained,
    means = means
  )
}

#' Estimate Means from a Training Set for Centering.
#' 
#' For a training set of data, this function computes the sample mean across numeric columns that require centering. Note that no data are centered by this function; see  \code{\link{process.center_step}}.
#' 
#' @param x a \code{center_step} object that specifies which columns will be centered.
#' @param data a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{center_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export

learn.center_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  means <- vapply(data[, col_names], mean, c(mean = 0), na.rm = TRUE)
  step_center_new(terms = x$terms, role = x$role, trained = TRUE, means = means)
}

#' Center Variables in a Data Set.
#' 
#' For a trained \code{center_step} object, this function will subtract the sample mean obtained from the training set from column in the current data. 
#' 
#' @param x A trained \code{center_step} object.
#' @param data A tibble or data frame that has numeric variables that will be centered.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @importFrom tibble as_tibble

process.center_step <- function(x, data, ...) {
  data[, names(x$means)] <- sweep(as.matrix(data[, names(x$means)]), 2, x$means, "-")
  as_tibble(data)
}

#' @export
print.center_step <- function(x, form_width = 30, ...) {
  cat("Centering with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

