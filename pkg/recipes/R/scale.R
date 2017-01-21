#' Declare Variables for Scaling
#' 
#' This function is a \emph{specification} of a recipe step that will normalize numeric data to have a standard deviation of one 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param sds A named numeric vector of standard deviations This is \code{NULL} until computed by \code{\link{learn.scale_step}}. 
#' @return An object of class \code{scale_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' 
step_scale <- function(recipe, terms, role = NA, trained = FALSE, sds = NULL) {
  add_step(
    recipe, 
    step_scale_new(
      terms = terms, 
      role = role,
      trained = trained, 
      sds = sds
    )
  )
}

step_scale_new <- function(terms = NULL, role = NA, trained = FALSE, sds = NULL) {
  step(
    subclass = "scale", 
    terms = terms,
    role = role,
    trained = trained,
    sds = sds
  )
}

#' Estimate Standard Deviations from a Training Set for Scaling
#' 
#' For a training set of data, this function computes the sample standard deviation across numeric columns that require scaling Note that no data are scaled by this function; see  \code{\link{process.scale_step}}.
#' 
#' @param x a \code{scale_step} object that specifies which columns will be scaled
#' @param data a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{scale_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @importFrom stats sd

learn.scale_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  sds <- vapply(data[, col_names], sd, c(sd = 0), na.rm = TRUE)
  step_scale_new(terms = x$terms, role = x$role, trained = TRUE, sds)
}

#' Scale Variables in a Data Set.
#' 
#' For a trained \code{scale_step} object, this function will divide the sample standard deviation obtained from the training set into the columns in the current data. 
#' 
#' @param x A trained \code{scale_step} object.
#' @param data A tibble or data frame that has numeric variables that will be scaled
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @importFrom tibble as_tibble

process.scale_step <- function(x, data, ...) {
  data[, names(x$sds)] <- sweep(as.matrix(data[, names(x$sds)]), 2, x$sds, "-")
  as_tibble(data)
}

print.scale_step <- function(x, form_width = 30, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
