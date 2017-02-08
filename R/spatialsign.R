#' Spatial Sign Preprocessing.
#' 
#' \code{step_spatialsign} is a \emph{specification} of a recipe step that will convert numeric data into a projection on to a unit sphere. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used for the normalization.
#' @param role For model terms created by this step, what analysis role should they be assigned? 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_spatialsign} returns an object of class \code{step_spatialsign}. 
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

learn.step_spatialsign <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

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
