#' Logarithmic Transformation
#' 
#' \code{step_log} creates a \emph{specification} of a recipe step that will log transform data. 
#' 
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created. 
#' @param base A numeric value for the base. 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_log} and \code{learn.step_log} return objects of class \code{step_log}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_log <- function(recipe, terms, role = NA, trained = FALSE, base = exp(1), vars = NULL) {
  add_step(
    recipe, 
    step_log_new(
      terms = terms, 
      role = role,
      trained = trained, 
      base = base,
      vars = vars
    )
  )
}

step_log_new <- function(terms = NULL, role = NA, trained = FALSE, 
                         base = NULL, vars = NULL) {
  step(
    subclass = "log", 
    terms = terms,
    role = role,
    trained = trained, 
    base = base,
    vars = vars
  )
}

#' For a training set of data, \code{learn.step_log} configures the log transformation (by basically doing nothing). This function is \emph{not} intended to be directly called by the user. 
#' 
#' @param x a \code{step_log} object that specifies which columns will be transformed
#' @inheritParams learn.step_center
#' @export
#' @importFrom stats optimize
#' @rdname step_log

learn.step_log <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_log_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE, 
    base = x$base,
    vars = col_names
  )
}

#' \code{process.step_log} is used to transform columns on specific data sets. This replaces values in the original columns. This function is \emph{not} intended to be directly called by the user. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_log} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_log

process.step_log <- function(object, newdata, ...) {
  col_names <- object$vars
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- log(newdata[ , col_names[i] ], base = object$base)
  as_tibble(newdata)
}

#' @export
print.step_log <- function(x, form_width = 30, ...) {
  cat("Log transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
