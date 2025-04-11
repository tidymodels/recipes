#' Yeo-Johnson transformation
#'
#' `step_YeoJohnson()` creates a *specification* of a recipe step that will
#' transform data using a Yeo-Johnson transformation.
#'
#' @inheritParams step_center
#' @param lambdas A numeric vector of transformation values. This is `NULL`
#'   until computed by [prep()].
#' @param limits A length 2 numeric vector defining the range to compute the
#'   transformation parameter lambda.
#' @param num_unique An integer where data that have less possible values will
#'   not be evaluated for a transformation.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' The Yeo-Johnson transformation is very similar to the Box-Cox but does not
#' require the input variables to be strictly positive. In the package, the
#' partial log-likelihood function is directly optimized within a reasonable set
#' of transformation values (which can be changed by the user).
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
#' @references Yeo, I. K., and Johnson, R. A. (2000). A new family of power
#'   transformations to improve normality or symmetry. *Biometrika*.
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' yj_transform <- step_YeoJohnson(rec, all_numeric())
#'
#' yj_estimates <- prep(yj_transform, training = biomass_tr)
#'
#' yj_te <- bake(yj_estimates, biomass_te)
#'
#' plot(density(biomass_te$sulfur), main = "before")
#' plot(density(yj_te$sulfur), main = "after")
#'
#' tidy(yj_transform, number = 1)
#' tidy(yj_estimates, number = 1)
step_YeoJohnson <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    lambdas = NULL,
    limits = c(-5, 5),
    num_unique = 5,
    na_rm = TRUE,
    skip = FALSE,
    id = rand_id("YeoJohnson")
  ) {
    add_step(
      recipe,
      step_YeoJohnson_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        lambdas = lambdas,
        limits = sort(limits)[1:2],
        num_unique = num_unique,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_YeoJohnson_new <-
  function(terms, role, trained, lambdas, limits, num_unique, na_rm, skip, id) {
    step(
      subclass = "YeoJohnson",
      terms = terms,
      role = role,
      trained = trained,
      lambdas = lambdas,
      limits = limits,
      num_unique = num_unique,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_YeoJohnson <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_whole(x$num_unique, args = "num_unique")
  check_bool(x$na_rm, arg = "na_rm")
  if (!is.numeric(x$limits) || anyNA(x$limits) || length(x$limits) != 2) {
    cli::cli_abort(
      "{.arg limits} should be a numeric vector with two values,
                    not {.obj_type_friendly {x$limits}}"
    )
  }

  x$limits <- sort(x$limits)

  values <- vapply(
    training[, col_names],
    estimate_yj,
    c(lambda = 0),
    limits = x$limits,
    num_unique = x$num_unique,
    na_rm = x$na_rm
  )
  values <- values[!is.na(values)]
  step_YeoJohnson_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lambdas = values,
    limits = x$limits,
    num_unique = x$num_unique,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_YeoJohnson <- function(object, new_data, ...) {
  col_names <- names(object$lambdas)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- yj_transform(
      new_data[[col_name]],
      lambda = object$lambdas[col_name]
    )
  }

  new_data
}

#' @export
print.step_YeoJohnson <-
  function(x, width = max(20, options()$width - 39), ...) {
    title <- "Yeo-Johnson transformation on "
    print_step(names(x$lambdas), x$terms, x$trained, title, width)
    invisible(x)
  }

## computes the new data given a lambda
#' Internal Functions
#'
#' @keywords internal
#' @rdname recipes-internal
#' @export
yj_transform <- function(x, lambda, ind_neg = NULL, eps = 0.001) {
  if (is.na(lambda)) {
    return(x)
  }
  if (!inherits(x, "tbl_df") || is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  } else {
    if (!is.vector(x)) {
      x <- as.vector(x)
    }
  }
  # TODO case weights: can we use weights here?
  if (is.null(ind_neg)) {
    dat_neg <- x < 0
    ind_neg <- list(is = which(dat_neg), not = which(!dat_neg))
  }
  not_neg <- ind_neg[["not"]]
  is_neg <- ind_neg[["is"]]

  nn_trans <- function(x, lambda) {
    if (abs(lambda) < eps) {
      log(x + 1)
    } else {
      ((x + 1)^lambda - 1) / lambda
    }
  }

  ng_trans <- function(x, lambda) {
    if (abs(lambda - 2) < eps) {
      -log(-x + 1)
    } else {
      -((-x + 1)^(2 - lambda) - 1) / (2 - lambda)
    }
  }

  if (length(not_neg) > 0) {
    x[not_neg] <- nn_trans(x[not_neg], lambda)
  }

  if (length(is_neg) > 0) {
    x[is_neg] <- ng_trans(x[is_neg], lambda)
  }
  x
}

## Helper for the log-likelihood calc for eq 3.1 of Yeo, I. K.,
## & Johnson, R. A. (2000). A new family of power transformations
## to improve normality or symmetry. Biometrika. page 957
ll_yj <- function(lambda, y, ind_neg, const, eps = 0.001) {
  n <- length(y)
  y_t <- yj_transform(y, lambda, ind_neg)
  var_t <- var(y_t) * (n - 1) / n
  res <- -.5 * n * log(var_t) + (lambda - 1) * const
  res
}

## eliminates missing data and returns -llh
yj_obj <- function(lam, dat, ind_neg, const) {
  ll_yj(lambda = lam, y = dat, ind_neg = ind_neg, const = const)
}

## estimates the values
#' @keywords internal
#' @rdname recipes-internal
#' @export
estimate_yj <- function(
  dat,
  limits = c(-5, 5),
  num_unique = 5,
  na_rm = TRUE,
  call = caller_env(2)
) {
  na_rows <- which(is.na(dat))
  if (length(na_rows) > 0) {
    if (na_rm) {
      dat <- dat[-na_rows]
    } else {
      cli::cli_abort(
        c(
          x = "Missing values are not allowed for the YJ transformation.",
          i = "See {.arg na_rm} option."
        ),
        call = call
      )
    }
  }

  eps <- .001
  if (length(unique(dat)) < num_unique) {
    return(NA)
  }
  dat_neg <- dat < 0
  ind_neg <- list(is = which(dat_neg), not = which(!dat_neg))

  const <- sum(sign(dat) * log(abs(dat) + 1))

  res <- optimize(
    yj_obj,
    interval = limits,
    maximum = TRUE,
    dat = dat,
    ind_neg = ind_neg,
    const = const,
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
tidy.step_YeoJohnson <- tidy.step_BoxCox
