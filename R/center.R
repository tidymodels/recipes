#' Centering Numeric Data
#' 
#' \code{step_center} creates a \emph{specification} of a recipe step that will normalize numeric data to have a mean of zero. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A formula that represents the variables or terms that will be processed. The raw variable names can be used or \pkg{dplyr} selection tools. See \code{\link{selections}} for more details. 
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. 
#' @param means A named numeric vector of means. This is \code{NULL} until computed by \code{\link{learn.recipe}}. 
#' @return \code{step_center} returns an object of class \code{step_center}.
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

learn.step_center <- function(x, training, info = NULL, na.rm = TRUE, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = na.rm)
  step_center_new(terms = x$terms, role = x$role, trained = TRUE, means = means)
}

process.step_center <- function(object, newdata, ...) {
  newdata[, names(object$means)] <- sweep(as.matrix(newdata[, names(object$means)]), 2, object$means, "-")
  as_tibble(newdata)
}

print.step_center <- function(x, form_width = 30, ...) {
  cat("Centering with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

