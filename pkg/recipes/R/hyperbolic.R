#' Hyperbolic Transformations
#' 
#' \code{step_hyperbolic} creates a \emph{specification} of a recipe step that will transform data using a hyperbolic function. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be transformed.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param func A character value for the function. Valid values are "sin", "cos", or "tan".
#' @param inverse A logical: should the inverse function be used? 
#' @return \code{step_hyperbolic} and \code{learn.step_hyperbolic} return objects of class \code{step_hyperbolic}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_hyperbolic <- function(recipe, terms, role = NA, trained = FALSE, func = "sin", inverse = TRUE) {
  funcs <- c("sin", "cos", "tan")
  if(!(func %in% funcs))
    stop("`func` should be either `sin``, `cos`, or `tan`")
  
  add_step(
    recipe, 
    step_hyperbolic_new(
      terms = terms, 
      role = role,
      trained = trained, 
      func = func,
      inverse = inverse
    )
  )
}

step_hyperbolic_new <- function(terms = NULL, role = NA, trained = FALSE, 
                                func = NULL, inverse = NULL) {
  step(
    subclass = "hyperbolic", 
    terms = terms,
    role = role,
    trained = trained, 
    func = func,
    inverse = inverse
  )
}

#' For a training set of data, \code{learn.step_hyperbolic} configures the hyperbolic transformation (by basically doing nothing). 
#' 
#' @param x a \code{step_hyperbolic} object that specifies which columns will be transformed
#' @param training a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @importFrom stats optimize
#' @rdname step_hyperbolic

learn.step_hyperbolic <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  step_hyperbolic_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE, 
    func = x$func,
    inverse = x$inverse
  )
}

#' \code{process.step_hyperbolic} is used to transform columns on specific data sets. This replaces values in the original columns. 
#' 
#' @param object A trained step object.
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_hyperbolic} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_hyperbolic

process.step_hyperbolic <- function(object, newdata, ...) {
  func <- if(object$inverse)
    get(paste0("a", object$func)) else 
      get(object$func)
  col_names <- filter_terms(object$terms, newdata) 
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- func(newdata[ , col_names[i] ])
  as_tibble(newdata)
}

#' @export
print.step_hyperbolic <- function(x, form_width = 30, ...) {
  ttl <- paste("Hyperbolic", x$func)
  if(x$inverse) ttl <- paste(ttl, "(inv)")
  cat(ttl, "transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
