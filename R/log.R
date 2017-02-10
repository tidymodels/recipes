#' Logarithmic Transformation
#' 
#' \code{step_log} creates a \emph{specification} of a recipe step that will log transform data. 
#' 
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created. 
#' @param base A numeric value for the base. 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_log} returns an object of class \code{step_log}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples 
#' set.seed(313)
#' examples <- matrix(exp(rnorm(40)), ncol = 2)
#' examples <- as.data.frame(examples)
#' 
#' rec <- recipe(~ V1 + V2, data = examples)
#' 
#' library(magrittr)
#' log_trans <- rec  %>%
#'   step_log(terms = ~ is_predictor())
#' 
#' log_obj <- learn(log_trans, training = examples)
#' 
#' transformed_te <- process(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)

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

process.step_log <- function(object, newdata, ...) {
  col_names <- object$vars
  for(i in seq_along(col_names))
    newdata[ , col_names[i] ] <- log(newdata[ , col_names[i] ], base = object$base)
  as_tibble(newdata)
}

print.step_log <- function(x, form_width = 30, ...) {
  cat("Log transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
