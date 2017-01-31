#' Square Root Transformation
#' 
#' \code{step_sqrt} creates a \emph{specification} of a recipe step that will square root transform the data. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be transformed.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @return \code{step_sqrt} and \code{learn.step_sqrt} return objects of class \code{step_sqrt}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_sqrt <- function(recipe, terms, role = NA, trained = FALSE) {
  add_step(
    recipe, 
    step_sqrt_new(
      terms = terms, 
      role = role,
      trained = trained
    )
  )
}

step_sqrt_new <- function(terms = NULL, role = NA, trained = FALSE) {
  step(
    subclass = "sqrt", 
    terms = terms,
    role = role,
    trained = trained
  )
}

#' For a training set of data, \code{learn.step_sqrt} configures the square root transformation (by basically doing nothing). 
#' 
#' @param x a \code{step_sqrt} object that specifies which columns will be transformed
#' @param training a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @importFrom stats optimize
#' @rdname step_sqrt

learn.step_sqrt <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  step_sqrt_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE
  )
}

#' \code{process.step_sqrt} is used to transform columns on specific data sets. This replaces values in the original columns. 
#' 
#' @param object A trained step object.
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_sqrt} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_sqrt

process.step_sqrt <- function(object, newdata, ...) {
  col_names <- filter_terms(object$terms, newdata) 
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- sqrt(newdata[ , col_names[i] ])
  as_tibble(newdata)
}

#' @export
print.step_sqrt <- function(x, form_width = 30, ...) {
  cat("Square root transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
