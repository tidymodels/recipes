#' Logit Transformation
#' 
#' \code{step_logit} creates a \emph{specification} of a recipe step that will logit transform the data. 
#' 
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created. 
#' @return \code{step_logit} and \code{learn.step_logit} return objects of class \code{step_logit}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_logit <- function(recipe, terms, role = NA, trained = FALSE) {
  add_step(
    recipe, 
    step_logit_new(
      terms = terms, 
      role = role,
      trained = trained
    )
  )
}

step_logit_new <- function(terms = NULL, role = NA, trained = FALSE) {
  step(
    subclass = "logit", 
    terms = terms,
    role = role,
    trained = trained
  )
}

#' For a training set of data, \code{learn.step_logit} configures the logit transformation (by basically doing nothing). 
#' 
#' @param x a \code{step_logit} object that specifies which columns will be transformed
#' @inheritParams learn.step_center
#' @export
#' @importFrom stats optimize
#' @rdname step_logit

learn.step_logit <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  step_logit_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE
  )
}

#' \code{process.step_logit} is used to transform columns on specific data sets. This replaces values in the original columns. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_logit} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_logit

process.step_logit <- function(object, newdata, ...) {
  col_names <- filter_terms(object$terms, newdata) 
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- 
      binomial()$linkfun(unlist(newdata[, col_names[i]], use.names = FALSE))
  as_tibble(newdata)
}

#' @export
print.step_logit <- function(x, form_width = 30, ...) {
  cat("Logit transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
