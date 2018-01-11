#' Yeo-Johnson Transformation
#'
#' `step_YeoJohnson` creates a *specification* of a
#'  recipe step that will transform data using a simple Yeo-Johnson
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
#' @details The Yeo-Johnson transformation is very similar to the
#'  Box-Cox but does not require the input variables to be strictly
#'  positive. In the package, the partial log-likelihood function is
#'  directly optimized within a reasonable set of transformation
#'  values (which can be changed by the user).
#'
#' This transformation is typically done on the outcome variable
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
#' @references Yeo, I. K., and Johnson, R. A. (2000). A new family of power
#'   transformations to improve normality or symmetry. *Biometrika*.
#' @examples
#'
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' yj_trans <- step_YeoJohnson(rec,  all_numeric())
#'
#' yj_estimates <- prep(yj_trans, training = biomass_tr)
#'
#' yj_te <- bake(yj_estimates, biomass_te)
#'
#' plot(density(biomass_te$sulfur), main = "before")
#' plot(density(yj_te$sulfur), main = "after")
#'
#' tidy(yj_trans, number = 1)
#' tidy(yj_estimates, number = 1)
#' @seealso [step_BoxCox()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]
step_YeoJohnson <-
  function(recipe, ..., role = NA, trained = FALSE,
           lambdas = NULL, limits = c(-5, 5), nunique = 5,
           na.rm = TRUE,
           skip = FALSE) {
    add_step(
      recipe,
      step_YeoJohnson_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        lambdas = lambdas,
        limits = sort(limits)[1:2],
        nunique = nunique,
        na.rm = na.rm,
        skip = skip
      )
    )
  }

step_YeoJohnson_new <-
  function(terms = NULL, role = NA, trained = FALSE,
           lambdas = NULL, limits = NULL, nunique = NULL,
           na.rm = NULL,
           skip = FALSE) {
    step(
      subclass = "YeoJohnson",
      terms = terms,
      role = role,
      trained = trained,
      lambdas = lambdas,
      limits = limits,
      nunique = nunique,
      na.rm = na.rm,
      skip = skip
    )
  }

#' @export
prep.step_YeoJohnson <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  values <- vapply(
    training[, col_names],
    estimate_yj,
    c(lambda = 0),
    limits = x$limits,
    nunique = x$nunique,
    na.rm = x$na.rm
  )
  values <- values[!is.na(values)]
  step_YeoJohnson_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lambdas = values,
    limits = x$limits,
    nunique = x$nunique,
    na.rm = x$na.rm,
    skip = x$skip
  )
}

#' @export
bake.step_YeoJohnson <- function(object, newdata, ...) {
  if (length(object$lambdas) == 0)
    return(as_tibble(newdata))
  param <- names(object$lambdas)
  for (i in seq_along(object$lambdas))
    newdata[, param[i]] <-
    yj_trans(getElement(newdata, param[i]),
             lambda = object$lambdas[param[i]])
  as_tibble(newdata)
}

print.step_YeoJohnson <-
  function(x, width = max(20, options()$width - 39), ...) {
    cat("Yeo-Johnson transformation on ", sep = "")
    printer(names(x$lambdas), x$terms, x$trained, width = width)
    invisible(x)
  }

## computes the new data given a lambda
#' Internal Functions
#'
#' These are not to be used directly by the users.
#' @export
#' @keywords internal
#' @rdname recipes-internal
yj_trans <- function(x, lambda, eps = .001) {
  if (is.na(lambda))
    return(x)
  if (!inherits(x, "tbl_df") || is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  } else {
    if (!is.vector(x))
      x <- as.vector(x)
  }

  not_neg <- which(x >= 0)
  is_neg <- which(x < 0)

  nn_trans <- function(x, lambda)
    if (abs(lambda) < eps)
      log(x + 1)
  else
    ((x + 1) ^ lambda - 1) / lambda

  ng_trans <- function(x, lambda)
    if (abs(lambda - 2) < eps)
      - log(-x + 1)
  else
    - ((-x + 1) ^ (2 - lambda) - 1) / (2 - lambda)

  if (length(not_neg) > 0)
    x[not_neg] <- nn_trans(x[not_neg], lambda)

  if (length(is_neg) > 0)
    x[is_neg] <- ng_trans(x[is_neg], lambda)
  x
}


## Helper for the log-likelihood calc for eq 3.1 of Yeo, I. K.,
## & Johnson, R. A. (2000). A new family of power transformations
## to improve normality or symmetry. Biometrika. page 957

#' @importFrom stats var
ll_yj <- function(lambda, y, eps = .001) {
  y <- y[!is.na(y)]
  n <- length(y)
  nonneg <- all(y > 0)
  y_t <- yj_trans(y, lambda)
  mu_t <- mean(y_t)
  var_t <- var(y_t) * (n - 1) / n
  const <- sum(sign(y) * log(abs(y) + 1))
  res <- -.5 * n * log(var_t) + (lambda - 1) * const
  res
}

#' @importFrom  stats complete.cases
## eliminates missing data and returns -llh
yj_obj <- function(lam, dat){
  dat <- dat[complete.cases(dat)]
  ll_yj(lambda = lam, y = dat)
}

## estimates the values
#' @importFrom stats optimize
#' @export
#' @keywords internal
#' @rdname recipes-internal
estimate_yj <- function(dat, limits = c(-5, 5), nunique = 5,
                        na.rm = TRUE) {
  na_rows <- which(is.na(dat))
  if (length(na_rows) > 0) {
    if (na.rm) {
      dat <- dat[-na_rows]
    } else {
      stop("Missing values in data. See `na.rm` option", call. = FALSE)
    }
  }

  eps <- .001
  if (length(unique(dat)) < nunique)
    return(NA)
  res <- optimize(
    yj_obj,
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


#' @rdname step_YeoJohnson
#' @param x A `step_YeoJohnson` object.
tidy.step_YeoJohnson <- tidy.step_BoxCox
