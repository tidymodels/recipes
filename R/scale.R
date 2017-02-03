#' Scaling Numeric Data
#' 
#' \code{step_scale} creates a \emph{specification} of a recipe step that will normalize numeric data to have a standard deviation of one. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created. 
#' @param sds A named numeric vector of standard deviations This is \code{NULL} until computed by \code{\link{learn.step_scale}}. 
#' @return \code{step_scale} and \code{learn.step_scale} return objects of class \code{step_scale}.
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

#' For a training set of data, \code{learn.step_scale} estimates the standard deviations from numeric columns. 
#' 
#' @param x a \code{step_scale} object that specifies which columns will be scaled
#' @inheritParams learn.step_center
#' @export
#' @importFrom stats sd
#' @rdname step_scale

learn.step_scale <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = TRUE)
  step_scale_new(terms = x$terms, role = x$role, trained = TRUE, sds)
}

#' \code{process.step_scale} is used to perform the scaling on specific data sets. This replaces values in the original columns. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be scaled
#' @return \code{process.step_scale} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_scale

process.step_scale <- function(object, newdata, ...) {
  newdata[, names(object$sds)] <- sweep(as.matrix(newdata[, names(object$sds)]), 2, object$sds, "/")
  as_tibble(newdata)
}

#' @export
print.step_scale <- function(x, form_width = 30, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
