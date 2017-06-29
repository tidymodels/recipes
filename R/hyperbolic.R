#' Hyperbolic Transformations
#'
#' \code{step_hyperbolic} creates a \emph{specification} of a recipe step that
#'   will transform data using a hyperbolic function.
#'
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created.
#' @param func A character value for the function. Valid values are "sin",
#'   "cos", or "tan".
#' @param inverse A logical: should the inverse function be used?
#' @param vars A character string of variable names that will be (eventually)
#'   populated by the \code{terms} argument.
#' @return \code{step_hyperbolic} returns an object of class
#'   \code{step_hyperbolic}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples
#' set.seed(313)
#' examples <- matrix(rnorm(40), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' cos_trans <- rec  %>%
#'   step_hyperbolic(all_predictors(),
#'                   func = "cos", inverse = FALSE)
#'
#' cos_obj <- prepare(cos_trans, training = examples)
#'
#' transformed_te <- bake(cos_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' @seealso \code{\link{step_logit}} \code{\link{step_invlogit}}
#'   \code{\link{step_log}}  \code{\link{step_sqrt}} \code{\link{recipe}}
#'   \code{\link{prepare.recipe}} \code{\link{bake.recipe}}

step_hyperbolic <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           func = "sin",
           inverse = TRUE,
           vars = NULL) {
    funcs <- c("sin", "cos", "tan")
    if (!(func %in% funcs))
      stop("`func` should be either `sin``, `cos`, or `tan`", call. = FALSE)
    add_step(
      recipe,
      step_hyperbolic_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        func = func,
        inverse = inverse,
        vars = vars
      )
    )
  }

step_hyperbolic_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           func = NULL,
           inverse = NULL,
           vars = NULL) {
    step(
      subclass = "hyperbolic",
      terms = terms,
      role = role,
      trained = trained,
      func = func,
      inverse = inverse,
      vars = vars
    )
  }

#' @export
prepare.step_hyperbolic <- function(x, training, info = NULL, ...) {
  col_names <- select_terms(x$terms, info = info)
  step_hyperbolic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    func = x$func,
    inverse = x$inverse,
    vars = col_names
  )
}

#' @export
bake.step_hyperbolic <- function(object, newdata, ...) {
  func <- if (object$inverse)
    get(paste0("a", object$func))
  else
    get(object$func)
  col_names <- object$vars
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <-
    func(getElement(newdata, col_names[i]))
  as_tibble(newdata)
}

print.step_hyperbolic <-
  function(x, width = max(20, options()$width - 32), ...) {
    ttl <- paste("Hyperbolic", x$func)
    if (x$inverse)
      ttl <- paste(ttl, "(inv)")
    cat(ttl, "transformation on ")
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
