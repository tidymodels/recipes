#' Spatial Sign Preprocessing.
#' 
#' \code{step_spatialsign} is a \emph{specification} of a recipe step that will convert numeric data into a projection on to a unit sphere. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used for the normalization.
#' @param role For model terms created by this step, what analysis role should they be assigned? 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @return \code{step_spatialsign} and \code{learn.spatialsign_step} return objects of class \code{spatialsign_step}. 
#' @keywords datagen
#' @concept preprocessing projection_methods
#' @export

step_spatialsign <- function(recipe, 
                             terms, 
                             role = "predictor",
                             trained = FALSE) {
  add_step(
    recipe, 
    step_spatialsign_new(
      terms = terms, 
      role = role,
      trained = trained
    )
  )
}

step_spatialsign_new <- function(terms = NULL, 
                                 role = "predictor",
                                 trained = FALSE) {
  
  step(
    subclass = "spatialsign",
    terms = terms,
    role = role,
    trained = trained
  )
}

#' \code{learn.spatialsign_step} processes a training data set for the transformation. 
#' @param x a \code{spatialsign_step} object that contains the spatial sign objects.
#' @param training a tibble or data frame that contains the training set. These data will be used to compute the loadings that are used when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @rdname step_spatialsign

learn.spatialsign_step <- function(x, training, ...) {
  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE
  )
}

#' To project the data onto a unit sphere, \code{process.spatialsign_step} is used and this overwrites the original columns.
#' @param object A trained step object.
#' @param newdata A tibble or data frame that has numeric variables that will be processed.
#' @return  \code{process.spatialsign_step} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom stats predict
#' @rdname step_spatialsign

process.spatialsign_step <- function(object, newdata, ...) {
  col_names <- filter_terms(object$terms, newdata) 
  ss <- function(x) x/sqrt(sum(x^2))
  newdata[, col_names] <- t(apply(as.matrix(newdata[, col_names]), 1, ss))
  as_tibble(newdata)
}

print.spatialsign_step <- function(x, form_width = 30, ...) {
  cat("Spatial sign on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
