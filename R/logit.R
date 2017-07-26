#' Logit Transformation
#'
#' \code{step_logit} creates a \emph{specification} of a recipe step that will
#'   logit transform the data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param columns A character string of variable names that will be (eventually)
#'   populated by the \code{terms} argument.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @details The inverse logit transformation takes values between zero and one
#'   and translates them to be on the real line using the function
#'   \code{f(p) = log(p/(1-p))}.
#' @examples
#' set.seed(313)
#' examples <- matrix(runif(40), ncol = 2)
#' examples <- data.frame(examples)
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' logit_trans <- rec  %>%
#'   step_logit(all_predictors())
#'
#' logit_obj <- prep(logit_trans, training = examples)
#'
#' transformed_te <- bake(logit_obj, examples)
#' plot(examples$X1, transformed_te$X1)
#' @seealso \code{\link{step_invlogit}} \code{\link{step_log}}
#' \code{\link{step_sqrt}}  \code{\link{step_hyperbolic}} \code{\link{recipe}}
#' \code{\link{prep.recipe}} \code{\link{bake.recipe}}

step_logit <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL) {
    add_step(recipe,
             step_logit_new(
               terms = check_ellipses(...),
               role = role,
               trained = trained,
               columns = columns
             ))
  }

step_logit_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL) {
    step(
      subclass = "logit",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns
    )
  }

#' @export
prep.step_logit <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_logit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names
  )
}

#' @importFrom tibble as_tibble
#' @importFrom stats binomial
#' @export
bake.step_logit <- function(object, newdata, ...) {
  for (i in seq_along(object$columns))
    newdata[, object$columns[i]] <-
      binomial()$linkfun(getElement(newdata, object$columns[i]))
  as_tibble(newdata)
}


print.step_logit <-
  function(x, width = max(20, options()$width - 33), ...) {
    cat("Logit transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }
