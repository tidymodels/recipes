#' Spatial Sign Preprocessing.
#' 
#' \code{step_spatialsign} is a \emph{specification} of a recipe step that will convert numeric data into a projection on to a unit sphere. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used for the normalization.
#' @param role For model terms created by this step, what analysis role should they be assigned? 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_spatialsign} and \code{learn.step_spatialsign} return objects of class \code{step_spatialsign}. 
#' @keywords datagen
#' @concept preprocessing projection_methods
#' @export

step_spatialsign <- function(recipe, 
                             terms, 
                             role = "predictor",
                             trained = FALSE, 
                             vars = NULL) {
  add_step(
    recipe, 
    step_spatialsign_new(
      terms = terms, 
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_spatialsign_new <- function(terms = NULL, 
                                 role = "predictor",
                                 trained = FALSE, 
                                 vars = NULL) {
  
  step(
    subclass = "spatialsign",
    terms = terms,
    role = role,
    trained = trained,
    vars = vars
  )
}

#' \code{learn.step_spatialsign} processes a training data set for the transformation. This function is \emph{not} intended to be directly called by the user. 
#' @param x a \code{step_spatialsign} object that contains the spatial sign objects.
#' @inheritParams learn.step_center
#' @export
#' @rdname step_spatialsign

learn.step_spatialsign <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

#' To project the data onto a unit sphere, \code{process.step_spatialsign} is used and this overwrites the original columns. This function is \emph{not} intended to be directly called by the user. 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be processed.
#' @return  \code{process.step_spatialsign} returns a tibble of processed data. 
#' @importFrom tibble as_tibble
#' @importFrom stats predict
#' @export
#' @rdname step_spatialsign

process.step_spatialsign <- function(object, newdata, ...) {
  col_names <- object$vars
  ss <- function(x) x/sqrt(sum(x^2))
  newdata[, col_names] <- t(apply(as.matrix(newdata[, col_names]), 1, ss))
  as_tibble(newdata)
}

print.step_spatialsign <- function(x, form_width = 30, ...) {
  cat("Spatial sign on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
