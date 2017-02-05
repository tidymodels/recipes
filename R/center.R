#' Centering Numeric Data
#' 
#' \code{step_center} creates a \emph{specification} of a recipe step that will normalize numeric data to have a mean of zero. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A formula that represents the variables or terms that will be processed. The raw variable names can be used or \pkg{dplyr} selection tools. See \code{\link{selections}} for more details. 
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. 
#' @param means A named numeric vector of means. This is \code{NULL} until computed by \code{\link{learn.step_center}}. 
#' @return \code{step_center} and \code{learn.step_center} return objects of class \code{step_center}.
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

#' For a training set of data, \code{learn.step_center} estimates the means for numeric columns. This function is \emph{not} intended to be directly called by the user. 
#' 
#' @param x a \code{step_center} object that specifies which columns will be centered.
#' @param training A tibble or data frame that contains the training set.
#' @param info A tibble with information on the current set of columns in the design matrix. 
#' @param na.rm A boolean indicates whether to remove NAs, default TRUE.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @rdname step_center

learn.step_center <- function(x, training, info = NULL, na.rm = TRUE, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = na.rm)
  step_center_new(terms = x$terms, role = x$role, trained = TRUE, means = means)
}

#' \code{process.step_center} is used to center the columns in specific data sets. This replaces values in the original columns. This function is \emph{not} intended to be directly called by the user. 
#' 
#' @param object A trained step object.
#' @param newdata A tibble or data frame that has numeric variables that will be centered.
#' @return \code{process.step_center} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_center

process.step_center <- function(object, newdata, ...) {
  newdata[, names(object$means)] <- sweep(as.matrix(newdata[, names(object$means)]), 2, object$means, "-")
  as_tibble(newdata)
}

#' @export
print.step_center <- function(x, form_width = 30, ...) {
  cat("Centering with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

