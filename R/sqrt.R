#' Square Root Transformation
#'
#' \code{step_sqrt} creates a \emph{specification} of a recipe step that will
#'   square root transform the data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'   transformed. See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param columns A character string of variable names that will be (eventually)
#'   populated by the \code{terms} argument.
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
#' sqrt_trans <- rec  %>%
#'   step_sqrt(all_predictors())
#'
#' sqrt_obj <- prep(sqrt_trans, training = examples)
#'
#' transformed_te <- bake(sqrt_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' @seealso \code{\link{step_logit}} \code{\link{step_invlogit}}
#'   \code{\link{step_log}}  \code{\link{step_hyperbolic}} \code{\link{recipe}}
#'   \code{\link{prep.recipe}} \code{\link{bake.recipe}}

step_sqrt <- function(recipe, ..., role = NA, trained = FALSE, columns = NULL) {
  add_step(
    recipe,
    step_sqrt_new(
      terms = check_ellipses(...),
      role = role,
      trained = trained,
      columns = columns
    )
  )
}

step_sqrt_new <-
  function(terms = NULL, role = NA, trained = FALSE, columns = NULL) {
    step(
      subclass = "sqrt",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns
    )
  }


#' @export
prep.step_sqrt <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_sqrt_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names
  )
}

#' @export
bake.step_sqrt <- function(object, newdata, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <-
      sqrt(getElement(newdata, col_names[i]))
  as_tibble(newdata)
}

print.step_sqrt <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Square root transformation on ", sep = "")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

