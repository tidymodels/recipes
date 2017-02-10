#' Square Root Transformation
#' 
#' \code{step_sqrt} creates a \emph{specification} of a recipe step that will square root transform the data. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be transformed.
#' @param role Not used by this step since no new variables are created. 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_sqrt}  returns an object of class \code{step_sqrt}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples 
#' set.seed(313)
#' examples <- matrix(rnorm(40)^2, ncol = 2)
#' examples <- as.data.frame(examples)
#' 
#' rec <- recipe(~ V1 + V2, data = examples)
#' 
#' library(magrittr)
#' sqrt_trans <- rec  %>%
#'   step_sqrt(terms = ~ is_predictor())
#' 
#' sqrt_obj <- learn(sqrt_trans, training = examples)
#' 
#' transformed_te <- process(sqrt_obj, examples)
#' plot(examples$V1, transformed_te$V1)

step_sqrt <- function(recipe, terms, role = NA, trained = FALSE, vars = NULL) {
  add_step(
    recipe, 
    step_sqrt_new(
      terms = terms, 
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_sqrt_new <- function(terms = NULL, role = NA, trained = FALSE, vars = NULL) {
  step(
    subclass = "sqrt", 
    terms = terms,
    role = role,
    trained = trained,
    vars = vars
  )
}


learn.step_sqrt <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_sqrt_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

process.step_sqrt <- function(object, newdata, ...) {
  col_names <- object$vars
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- sqrt(newdata[ , col_names[i] ])
  as_tibble(newdata)
}

print.step_sqrt <- function(x, form_width = 30, ...) {
  cat("Square root transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
