#' Square Root Transformation
#'
#' \code{step_sqrt} creates a \emph{specification} of a recipe step that will
#'   square root transform the data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables will be
#'   transformed. See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param vars A character string of variable names that will be (eventually)
#'   populated by the \code{terms} argument.
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
#' sqrt_trans <- rec  %>%
#'   step_sqrt(all_predictors())
#'
#' sqrt_obj <- prepare(sqrt_trans, training = examples)
#'
#' transformed_te <- bake(sqrt_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' @seealso \code{\link{step_logit}} \code{\link{step_invlogit}}
#'   \code{\link{step_log}}  \code{\link{step_hyperbolic}} \code{\link{recipe}}
#'   \code{\link{prepare.recipe}} \code{\link{bake.recipe}}

step_sqrt <- function(recipe, ..., role = NA, trained = FALSE, vars = NULL) {
  add_step(
    recipe,
    step_sqrt_new(
      terms = check_ellipses(...),
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_sqrt_new <-
  function(terms = NULL, role = NA, trained = FALSE, vars = NULL) {
    step(
      subclass = "sqrt",
      terms = terms,
      role = role,
      trained = trained,
      vars = vars
    )
  }


#' @export
prepare.step_sqrt <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_sqrt_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

#' @export
bake.step_sqrt <- function(object, newdata, ...) {
  col_names <- object$vars
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <-
      sqrt(getElement(newdata, col_names[i]))
  as_tibble(newdata)
}

print.step_sqrt <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Square root transformation on ", sep = "")
  if (x$trained) {
    cat(format_ch_vec(x$vars, width = width))
  } else
    cat(format_selectors(x$terms, wdth = width))
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}

