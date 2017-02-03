#' Yeo-Johnson Transformation
#' 
#' \code{step_YeoJohnson} creates a \emph{specification} of a recipe step that will transform data using a simple Yeo-Johnson transformation. 
#' 
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created. 
#' @param lambdas A numeric vector of transformation values. This is \code{NULL} until computed by \code{\link{learn.step_YeoJohnson}}. 
#' @param limits A length 2 numeric vector defining the range to compute the transformation parameter lambda. 
#' @param nunique An integer where data that have less possible values will not be evaluate for a transformation
#' @return \code{step_YeoJohnson} and \code{learn.step_YeoJohnson} return objects of class \code{step_YeoJohnson}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_YeoJohnson <- function(recipe, terms, role = NA, trained = FALSE, lambdas = NULL, limits = c(-5, 5), nunique = 5) {
  add_step(
    recipe, 
    step_YeoJohnson_new(
      terms = terms, 
      role = role,
      trained = trained, 
      lambdas = lambdas,
      limits = sort(limits)[1:2],
      nunique = nunique
    )
  )
}

step_YeoJohnson_new <- function(terms = NULL, role = NA, trained = FALSE, 
                            lambdas = NULL, limits = NULL, nunique = NULL) {
  step(
    subclass = "YeoJohnson", 
    terms = terms,
    role = role,
    trained = trained, 
    lambdas = lambdas,
    limits = limits,
    nunique = nunique
  )
}

#' For a training set of data, \code{learn.step_YeoJohnson} estimates the simple Yeo-Johnson transformation. 
#' 
#' @param x a \code{step_YeoJohnson} object that specifies which columns will be transformed
#' @inheritParams learn.step_center
#' @export
#' @importFrom stats optimize
#' @rdname step_YeoJohnson

learn.step_YeoJohnson <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  values <- vapply(
    training[, col_names], 
    estimate_yj, 
    c(lambda = 0), 
    limits = x$limits,
    nunique = x$nunique)
  values <- values[!is.na(values)]
  step_YeoJohnson_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE, 
    lambdas = values,
    limits = x$limits,
    nunique = x$nunique
  )
}

#' \code{process.step_YeoJohnson} is used to transform columns on specific data sets. This replaces values in the original columns. 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_YeoJohnson} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_YeoJohnson

process.step_YeoJohnson <- function(object, newdata, ...) {
  if(length(object$lambdas) == 0) 
    return(as_tibble(newdata))
  param <- names(object$lambdas)
  for(i in seq_along(object$lambdas))
    newdata[ , param[i] ] <- yj_trans(newdata[ , param[i] ], lambda = object$lambdas[ param[i] ])
  as_tibble(newdata)
}

#' @export
print.step_YeoJohnson <- function(x, form_width = 30, ...) {
  cat("Yeo-Johnson transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

## computes the new data
yj_trans <- function(x, lambda, eps = .001) {
  if(is.na(lambda)) return(x)
  if(!inherits(x, "tbl_df") || is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  } else {
    if(!is.vector(x)) x <- as.vector(x)
  }
  
  not_neg <- x >= 0
  
  nn_trans <- function(x, lambda)
    if(abs(lambda) < eps)
      log(x+1) else 
        ((x+1)^lambda -1)/lambda
  
  ng_trans <- function(x, lambda)
    if(abs(lambda-2) < eps)
      -log(-x+1) else 
        -((-x+1)^(2-lambda)-1)/(2-lambda)
  
  if(any(not_neg))
    x[not_neg] <- nn_trans(x[not_neg], lambda)
  if(any(!not_neg))
    x[!not_neg] <- ng_trans(x[!not_neg], lambda) 
  x
}


## Helper for the log-likelihood calc for eq 3.1 of Yeo, I. K., 
## & Johnson, R. A. (2000). A new family of power transformations 
## to improve normality or symmetry. Biometrika. page 957
ll_yj <- function(lambda, y, eps = .001) {
  n <- length(y)
  nonneg <- all(y > 0)
  y_t <- yj_trans(y, lambda)
  mu_t <- mean(y_t)
  var_t <- var(y_t)*(n-1)/n
  const <- sum(sign(y)*log(abs(y)+1))
  res <- -.5*n*log(var_t) + (lambda - 1) * const
  res
}

## eliminates missing data and returns -llh
yj_obj <- function(lam, dat){
  dat <- dat[complete.cases(dat)]
  ll_yj(lambda = lam, y = dat)
}

## estimates the values
estimate_yj <- function(dat, limits = c(-5, 5), nunique = 5) {
  eps <- .001
  if(length(unique(dat)) < nunique) 
    return(NA)
  res <- optimize(yj_obj, interval = limits,
                  maximum = TRUE, dat = dat, 
                  tol = .0001)
  lam <- res$maximum
  if(abs(limits[1]-lam) <= eps | abs(limits[2]-lam) <= eps)
    lam <- NA
  lam
}