#' Logit Transformation
#' 
#' \code{step_logit} creates a \emph{specification} of a recipe step that will logit transform the data. 
#' 
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created. 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_logit} returns an object of class \code{step_logit}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @details The inverse logit transformation takes values between zero and one and translates them to be on the real line using the function \code{f(p) = log(p/(1-p))}. 
#' @examples 
#' set.seed(313)
#' examples <- matrix(runif(40), ncol = 2)
#' examples <- data.frame(examples)
#' 
#' rec <- recipe(~ X1 + X2, data = examples)
#' 
#' library(magrittr)
#' logit_trans <- rec  %>%
#'   step_logit(terms = ~ is_predictor())
#' 
#' logit_obj <- learn(logit_trans, training = examples)
#' 
#' transformed_te <- process(logit_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#' @seealso \code{\link{step_invlogit}} \code{\link{step_log}}  \code{\link{step_sqrt}}  \code{\link{step_hyperbolic}}  \code{\link{recipe}} \code{\link{learn.recipe}} \code{\link{process.recipe}} 

step_logit <- function(recipe, terms, role = NA, trained = FALSE, vars = NULL) {
  add_step(
    recipe, 
    step_logit_new(
      terms = terms, 
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_logit_new <- function(terms = NULL, role = NA, trained = FALSE, vars = NULL) {
  step(
    subclass = "logit", 
    terms = terms,
    role = role,
    trained = trained, 
    vars = vars
  )
}

learn.step_logit <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_logit_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

#' @importFrom tibble as_tibble
#' @importFrom stats binomial
process.step_logit <- function(object, newdata, ...) {
  for(i in seq_along(object$vars))
    newdata[ , object$vars[i] ] <- 
      binomial()$linkfun(unlist(newdata[, object$vars[i] ], use.names = FALSE))
  as_tibble(newdata)
}

print.step_logit <- function(x, width = 30, ...) {
  cat("Logit transformation on ")
  cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
