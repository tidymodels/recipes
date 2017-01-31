#' Box-Cox Transformation for Non-Negative Data
#' 
#' \code{step_BoxCox} creates a \emph{specification} of a recipe step that will transform data using a simple Box-Cox transformation. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created. 
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param lambdas A numeric vector of transformation values. This is \code{NULL} until computed by \code{\link{learn.step_BoxCox}}. 
#' @param limits A length 2 numeric vector defining the range to compute the transformation parameter lambda. 
#' @param nunique An integer where data that have less possible values will not be evaluate for a transformation
#' @return \code{step_BoxCox} and \code{learn.step_BoxCox} return objects of class \code{step_BoxCox}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' 
step_BoxCox <- function(recipe, terms, role = NA, trained = FALSE, lambdas = NULL, limits = c(-5, 5), nunique = 5) {
  add_step(
    recipe, 
    step_BoxCox_new(
      terms = terms, 
      role = role,
      trained = trained, 
      lambdas = lambdas,
      limits = sort(limits)[1:2],
      nunique = nunique
    )
  )
}

step_BoxCox_new <- function(terms = NULL, role = NA, trained = FALSE, 
                            lambdas = NULL, limits = NULL, nunique = NULL) {
  step(
    subclass = "BoxCox", 
    terms = terms,
    role = role,
    trained = trained, 
    lambdas = lambdas,
    limits = limits,
    nunique = nunique
  )
}

#' For a training set of data, \code{learn.step_BoxCox} estimates the simple Box-Cox transformation. 
#' 
#' @param x a \code{step_BoxCox} object that specifies which columns will be transformed
#' @param training a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @importFrom stats optimize
#' @rdname step_BoxCox

learn.step_BoxCox <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  values <- vapply(
    training[, col_names], 
    estimate_bc, 
    c(lambda = 0), 
    limits = x$limits,
    nunique = x$nunique)
  values <- values[!is.na(values)]
  step_BoxCox_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE, 
    lambdas = values,
    limits = x$limits,
    nunique = x$nunique
  )
}

#' \code{process.step_BoxCox} is used to transform columns on specific data sets. This replaces values in the original columns. 
#' 
#' @param object A trained step object.
#' @param newdata A tibble or data frame that has numeric variables that will be transformed
#' @return \code{process.step_BoxCox} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @rdname step_BoxCox

process.step_BoxCox <- function(object, newdata, ...) {
  if(length(object$lambdas) == 0) 
    return(as_tibble(newdata))
  param <- names(object$lambdas)
  for(i in seq_along(object$lambdas))
    newdata[ , param[i] ] <- bc_trans(newdata[ , param[i] ], lambda = object$lambdas[i])
  as_tibble(newdata)
}

#' @export
print.step_BoxCox <- function(x, form_width = 30, ...) {
  cat("Box-Cox transformation on ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

## computes the new data
bc_trans <- function(x, lambda, eps = .001) {
  if(is.na(lambda)) return(x)
  if(abs(lambda) < eps)
    log(x) else (x^lambda -1)/lambda
}

## helper for the log-likelihood calc
ll_lam <- function(lambda, y, gm, eps = .001) {
  n <- length(y)
  gm0 <- gm^(lambda - 1)
  z <- if(abs(lambda) <= eps)
    log(y)/gm0 else 
      (y^lambda - 1)/(lambda*gm0)
  var_z <- var(z)*(n-1)/n
  -.5*n*log(var_z)
}

## eliminates missing data and returns -llh
bc_obj <- function(lam, dat){
  dat <- dat[complete.cases(dat)]
  geo_mean <- exp(mean(log(dat)))
  ll_lam(lambda = lam, y = dat, gm = geo_mean)
}

## estimates the values
estimate_bc <- function(dat, limits = c(-5, 5), nunique = 5) {
  eps <- .001
  if(length(unique(dat)) < nunique | any(dat[complete.cases(dat)] <= 0)) 
    return(NA)
  res <- optimize(bc_obj, interval = limits,
                  maximum = TRUE, dat = dat, 
                  tol = .0001)
  lam <- res$maximum
  if(abs(limits[1]-lam) <= eps | abs(limits[2]-lam) <= eps)
    lam <- NA
  lam
}




