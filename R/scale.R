#' Scaling Numeric Data
#' 
#' \code{step_scale} creates a \emph{specification} of a recipe step that will normalize numeric data to have a standard deviation of one. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created. 
#' @param sds A named numeric vector of standard deviations This is \code{NULL} until computed by \code{\link{learn.recipe}}. 
#' @return \code{step_scale}  returns an object of class \code{step_scale}.
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

#' @importFrom stats sd
learn.step_scale <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = TRUE)
  step_scale_new(terms = x$terms, role = x$role, trained = TRUE, sds)
}

process.step_scale <- function(object, newdata, ...) {
  newdata[, names(object$sds)] <- sweep(as.matrix(newdata[, names(object$sds)]), 2, object$sds, "/")
  as_tibble(newdata)
}

print.step_scale <- function(x, form_width = 30, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
