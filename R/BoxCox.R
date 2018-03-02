#' Box-Cox Transformation for Non-Negative Data
#'
#' `step_BoxCox` creates a *specification* of a recipe
#'  step that will transform data using a simple Box-Cox
#'  transformation.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param lambdas A numeric vector of transformation values. This
#'  is `NULL` until computed by [prep.recipe()].
#' @param limits A length 2 numeric vector defining the range to
#'  compute the transformation parameter lambda.
#' @param nunique An integer where data that have less possible
#'  values will not be evaluate for a transformation.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  lambda estimate).
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @details The Box-Cox transformation, which requires a strictly
#'  positive variable, can be used to rescale a variable to be more
#'  similar to a normal distribution. In this package, the partial
#'  log-likelihood function is directly optimized within a
#'  reasonable set of transformation values (which can be changed by
#'  the user).
#'
#'   This transformation is typically done on the outcome variable
#'  using the residuals for a statistical model (such as ordinary
#'  least squares). Here, a simple null model (intercept only) is
#'  used to apply the transformation to the *predictor*
#'  variables individually. This can have the effect of making the
#'  variable distributions more symmetric.
#'
#' If the transformation parameters are estimated to be very
#'  closed to the bounds, or if the optimization fails, a value of
#'  `NA` is used and no transformation is applied.
#'
#' @references Sakia, R. M. (1992). The Box-Cox transformation technique:
#'   A review. *The Statistician*, 169-178..
#' @examples
#'
#' rec <- recipe(~ ., data = as.data.frame(state.x77))
#'
#' bc_trans <- step_BoxCox(rec, all_numeric())
#'
#' bc_estimates <- prep(bc_trans, training = as.data.frame(state.x77))
#'
#' bc_data <- bake(bc_estimates, as.data.frame(state.x77))
#'
#' plot(density(state.x77[, "Illiteracy"]), main = "before")
#' plot(density(bc_data$Illiteracy), main = "after")
#'
#' tidy(bc_trans, number = 1)
#' tidy(bc_estimates, number = 1)
#'
#' @seealso [step_YeoJohnson()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]
step_BoxCox <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           lambdas = NULL,
           limits = c(-5, 5),
           nunique = 5,
           skip = FALSE) {
    add_step(
      recipe,
      step_BoxCox_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        lambdas = lambdas,
        limits = sort(limits)[1:2],
        nunique = nunique,
        skip = skip
      )
    )
  }

step_BoxCox_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           lambdas = NULL,
           limits = NULL,
           nunique = NULL,
           skip = FALSE) {
    step(
      subclass = "BoxCox",
      terms = terms,
      role = role,
      trained = trained,
      lambdas = lambdas,
      limits = limits,
      nunique = nunique,
      skip = skip
    )
  }

#' @export
prep.step_BoxCox <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  values <- vapply(
    training[, col_names],
    estimate_bc,
    c(lambda = 0),
    limits = x$limits,
    nunique = x$nunique
  )
  values <- values[!is.na(values)]
  step_BoxCox_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lambdas = values,
    limits = x$limits,
    nunique = x$nunique,
    skip = x$skip
  )
}

#' @export
bake.step_BoxCox <- function(object, newdata, ...) {
  if (length(object$lambdas) == 0)
    return(as_tibble(newdata))
  param <- names(object$lambdas)
  for (i in seq_along(object$lambdas))
    newdata[, param[i]] <-
    bc_trans(getElement(newdata, param[i]), lambda = object$lambdas[i])
  as_tibble(newdata)
}

print.step_BoxCox <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Box-Cox transformation on ", sep = "")
    printer(names(x$lambdas), x$terms, x$trained, width = width)
    invisible(x)
  }

## computes the new data
bc_trans <- function(x, lambda, eps = .001) {
  if (is.na(lambda))
    return(x)
  if (abs(lambda) < eps)
    log(x)
  else
    (x ^ lambda - 1) / lambda
}

## helper for the log-likelihood calc

#' @importFrom stats var
ll_bc <- function(lambda, y, gm, eps = .001) {
  n <- length(y)
  gm0 <- gm ^ (lambda - 1)
  z <- if (abs(lambda) <= eps)
    log(y) / gm0
  else
    (y ^ lambda - 1) / (lambda * gm0)
  var_z <- var(z) * (n - 1) / n
  - .5 * n * log(var_z)
}

#' @importFrom stats complete.cases
## eliminates missing data and returns -llh
bc_obj <- function(lam, dat) {
  dat <- dat[complete.cases(dat)]
  geo_mean <- exp(mean(log(dat)))
  ll_bc(lambda = lam, y = dat, gm = geo_mean)
}

#' @importFrom stats optimize
## estimates the values
estimate_bc <- function(dat,
                        limits = c(-5, 5),
                        nunique = 5) {
  eps <- .001
  if (length(unique(dat)) < nunique |
      any(dat[complete.cases(dat)] <= 0))
    return(NA)
  res <- optimize(
    bc_obj,
    interval = limits,
    maximum = TRUE,
    dat = dat,
    tol = .0001
  )
  lam <- res$maximum
  if (abs(limits[1] - lam) <= eps | abs(limits[2] - lam) <= eps)
    lam <- NA
  lam
}


#' @rdname step_BoxCox
#' @param x A `step_BoxCox` object.
tidy.step_BoxCox <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$lambdas),
                  value = x$lambdas)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res
}
