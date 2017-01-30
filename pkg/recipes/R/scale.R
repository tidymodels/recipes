#' Scaling Numeric Data
#' 
#' \code{step_scale} creates a \emph{specification} of a recipe step that will normalize numeric data to have a standard deviation of one. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param sds A named numeric vector of standard deviations This is \code{NULL} until computed by \code{\link{learn.scale_step}}. 
#' @return \code{step_scale} and \code{learn.scale_step} return objects of class \code{scale_step}.
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

#' For a training set of data, \code{learn.scale_step} estimates the standard deviations from numeric columns. 
#' 
#' @param x a \code{scale_step} object that specifies which columns will be scaled
#' @param training a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @importFrom stats sd
#' @rdname step_scale

learn.scale_step <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = TRUE)
  step_scale_new(terms = x$terms, role = x$role, trained = TRUE, sds)
}

#' \code{process.scale_step} is used to perform the scaling on specific data sets. This replaces values in the original columns. 
#' 
#' @param data A tibble or data frame that has numeric variables that will be scaled
#' @return \code{process.scale_step} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_scale

process.scale_step <- function(x, data, ...) {
  data[, names(x$sds)] <- sweep(as.matrix(data[, names(x$sds)]), 2, x$sds, "/")
  as_tibble(data)
}

#' @export
print.scale_step <- function(x, form_width = 30, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
