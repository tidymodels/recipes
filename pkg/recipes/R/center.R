#' Centering Numeric Data
#' 
#' \code{step_center} creates a \emph{specification} of a recipe step that will normalize numeric data to have a mean of zero. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be centered.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. 
#' @param means A named numeric vector of means. This is \code{NULL} until computed by \code{\link{learn.center_step}}. 
#' @return \code{step_center} and \code{learn.center_step} return objects of class \code{center_step}.
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

#' For a training set of data, \code{learn.center_step} estimates the means for numeric columns.
#' 
#' @param x a \code{center_step} object that specifies which columns will be centered.
#' @param training A tibble or data frame that contains the training set.
#' @param na.rm A boolean indicates whether to remove NAs, default TRUE.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @rdname step_center

learn.center_step <- function(x, training, na.rm = TRUE, ...) {
  col_names <- filter_terms(x$terms, training) 
  
  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = na.rm)
  step_center_new(terms = x$terms, role = x$role, trained = TRUE, means = means)
}

#' \code{process.center_step} is used to center the columns in specific data sets. This replaces values in the original columns. 
#' 
#' @param data A tibble or data frame that has numeric variables that will be centered.
#' @return \code{process.center_step} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_center

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

