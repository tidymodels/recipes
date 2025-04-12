#' Box-Cox transformation for non-negative data
#'
#' `step_BoxCox()` creates a *specification* of a recipe step that will
#' transform data using a Box-Cox transformation.
#'
#' @inheritParams step_center
#' @param lambdas A numeric vector of transformation values. This is `NULL`
#'   until computed by [prep()].
#' @param limits A length 2 numeric vector defining the range to compute the
#'   transformation parameter lambda.
#' @param num_unique An integer to specify minimum required unique values to
#'   evaluate for a transformation.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' The Box-Cox transformation, which requires a strictly positive variable, can
#' be used to rescale a variable to be more similar to a normal distribution. In
#' this package, the partial log-likelihood function is directly optimized
#' within a reasonable set of transformation values (which can be changed by the
#' user).
#'
#' This transformation is typically done on the outcome variable using the
#' residuals for a statistical model (such as ordinary least squares). Here, a
#' simple null model (intercept only) is used to apply the transformation to the
#' *predictor* variables individually. This can have the effect of making the
#' variable distributions more symmetric.
#'
#' If the transformation parameters are estimated to be very closed to the
#' bounds, or if the optimization fails, a value of `NA` is used and no
#' transformation is applied.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the lambda estimate}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @references Sakia, R. M. (1992). The Box-Cox transformation technique:
#'   A review. *The Statistician*, 169-178..
#' @examples
#'
#' rec <- recipe(~., data = as.data.frame(state.x77))
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
step_BoxCox <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    lambdas = NULL,
    limits = c(-5, 5),
    num_unique = 5,
    skip = FALSE,
    id = rand_id("BoxCox")
  ) {
    add_step(
      recipe,
      step_BoxCox_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        lambdas = lambdas,
        limits = sort(limits)[1:2],
        num_unique = num_unique,
        skip = skip,
        id = id
      )
    )
  }

step_BoxCox_new <-
  function(terms, role, trained, lambdas, limits, num_unique, skip, id) {
    step(
      subclass = "BoxCox",
      terms = terms,
      role = role,
      trained = trained,
      lambdas = lambdas,
      limits = limits,
      num_unique = num_unique,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_BoxCox <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  values <- vapply(
    training[, col_names],
    estimate_bc,
    c(lambda = 0),
    limits = x$limits,
    num_unique = x$num_unique
  )
  if (anyNA(values)) {
    var_names <- names(values[is.na(values)])
    cli::cli_warn(
      "No Box-Cox transformation could be estimated for: {.var {var_names}}."
    )
  }
  values <- values[!is.na(values)]
  step_BoxCox_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lambdas = values,
    limits = x$limits,
    num_unique = x$num_unique,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_BoxCox <- function(object, new_data, ...) {
  col_names <- names(object$lambdas)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- bc_trans(
      new_data[[col_name]],
      lambda = object$lambdas[col_name]
    )
  }

  new_data
}

#' @export
print.step_BoxCox <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Box-Cox transformation on "
    print_step(names(x$lambdas), x$terms, x$trained, title, width)
    invisible(x)
  }

## computes the new data
bc_trans <- function(x, lambda, eps = .001) {
  if (any(x <= 0)) {
    cli::cli_warn(
      "Applying Box-Cox transformation to non-positive data in column \\
      {.field {names(lambda)}}."
    )
  }

  if (is.na(lambda)) {
    return(x)
  }
  if (abs(lambda) < eps) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
}

## helper for the log-likelihood calc

# TODO case weights: Is there a weighted version of this likelihood?
ll_bc <- function(lambda, y, gm, eps = .001) {
  n <- length(y)
  gm0 <- gm^(lambda - 1)
  z <- if (abs(lambda) <= eps) {
    log(y) / gm0
  } else {
    (y^lambda - 1) / (lambda * gm0)
  }
  var_z <- var(z) * (n - 1) / n
  -.5 * n * log(var_z)
}

## eliminates missing data and returns -llh
bc_obj <- function(lam, dat, geo_mean) {
  ll_bc(lambda = lam, y = dat, gm = geo_mean)
}

## estimates the values
estimate_bc <- function(dat, limits = c(-5, 5), num_unique = 5) {
  eps <- .001
  if (length(unique(dat)) < num_unique) {
    cli::cli_warn("Fewer than {.arg num_unique} values in selected variable.")
    return(NA)
  } else if (any(dat <= 0)) {
    cli::cli_warn("Non-positive values in selected variable.")
    return(NA)
  }

  geo_mean <- exp(mean(log(dat)))

  res <- optimize(
    bc_obj,
    interval = limits,
    maximum = TRUE,
    dat = dat,
    geo_mean = geo_mean,
    tol = .0001
  )
  lam <- res$maximum
  if (abs(limits[1] - lam) <= eps | abs(limits[2] - lam) <= eps) {
    lam <- NA
  }
  lam
}

#' @rdname tidy.recipe
#' @export
tidy.step_BoxCox <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$lambdas),
      value = unname(x$lambdas)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}
