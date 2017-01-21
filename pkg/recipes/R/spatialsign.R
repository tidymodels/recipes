#' Declare Which Variables for the Spatial Sign.
#' 
#' This function is a \emph{specification} of a recipe step that will convert numeric data into a projection on to a unit sphere. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used for the normalization.
#' @param role For model terms created by this step, what analysis role should they be assigned? 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @return An object of class \code{spatialsign_step}. 
#' @author Max Kuhn
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

#' Setup a Training Set for the Spatial Sign
#' 
#' The spatial sign is a transformation of the original variables that does not require any parameters to be estimated. As such, this function only returns the original object. 
#' @param x a \code{spatialsign_step} object that contains the spatial sign objects.
#' @param data a tibble or data frame that contains the training set. These data will be used to compute the loadings that are used when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{spatialsign_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing projection_methods
learn.spatialsign_step <- function(x, data, ...) {
  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE
  )
}

#' Compute the Spatial Sign for a Data Set.
#' 
#' For a \code{spatialsign_step} object, this function projects the current data into a unit sphere. This creates overwrites the original columns
#' @param x A trained \code{spatialsign_step} object.
#' @param data A tibble or data frame that has numeric variables that will be transformed.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing projection_methods
#' @importFrom tibble as_tibble
#' @importFrom stats predict
#' @importFrom caret spatialSign
process.spatialsign_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  data[, col_names] <- spatialSign(data[, col_names])
  as_tibble(data)
}

print.spatialsign_step <- function(x, form_width = 30, ...) {
  cat("Spatial sign on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
