#' PCA Signal Extraction
#' 
#' \code{step_pca} creates a \emph{specification} of a recipe step that will convert numeric data into one or more principal components. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to compute the components.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new principal component columns created by the original variables will be used as predictors in a model. 
#' @param num The number of PCA components to retain as new predictors. If \code{num} is greater than the number of columns or the number of possible components, a smaller value will be used. 
#' @param options A list of options to \code{\link[stats]{prcomp}}. Defaults are set for the arguments \code{center} and \code{scale.} but others can be passed in (e.g. \code{tol}). \bold{Note} that the argument \code{x} should not be passed here (or at all).
#' @param res The \code{\link[stats]{prcomp}} object is stored here once this preprocessing step has be trained by \code{\link{learn.recipe}}.
#' @param prefix A character string that will be the prefix to the resulting new variables. See notes below
#' @return \code{step_pca}  returns an object of class \code{step_pca}. 
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @export
#' @details 
#' Principal component analysis (PCA) is a transformation of a group of variables that produces a new set of artificial features or components. These components are designed to capture the maximum amount of information (i.e. variance) in the original variables. Also, the components are statistically independent from one another. This means that they can be used to combat large inter-variables correlations in a data set. 
#' 
#' It is advisable to standardized the variables prior to running PCA. Here, each variable will be centered and scaled prior to the PCA calculation. This can be changed using the \code{options} argument. The means and standard deviation are estimated from the training set and are applied to any data sets that are converted to principal components. 
#' 
#' The argument \code{num} controls the number of components that will be retained (the original variables that are used to derive the components are removed from the data). The new components will have names that begin with \code{prefix} and a sequence of numbers. The variable names are padded with zeros. For example, if \code{num < 10}, their names will be \code{PC1} - \code{PC9}. If \code{num = 101}, the names would be \code{PC001} - \code{PC101}. 
#' 
#' @references Jolliffe, I. T. (2010). \emph{Principal Component Analysis}. Springer.
#' 
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' pca_trans <- step_pca(rec, terms = ~ is_numeric(), num = 3)
#' pca_estimates <- learn(pca_trans, training = USArrests)
#' pca_data <- process(pca_estimates, USArrests)
#' 
#' rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
#' plot(pca_data$PC1, pca_data$PC2,
#'      xlim = rng, ylim = rng)
#' @seealso \code{\link{step_ica}} \code{\link{step_kpca}} \code{\link{step_isomap}} \code{\link{recipe}} \code{\link{learn.recipe}} \code{\link{process.recipe}} 
step_pca <- function(recipe, 
                     terms, 
                     role = "predictor",
                     trained = FALSE,
                     num  = 5, 
                     options = list(center = TRUE, scale. = TRUE, retx = FALSE),
                     res = NULL,
                     prefix = "PC") {
  add_step(
    recipe, 
    step_pca_new(
      terms = terms, 
      role = role,
      trained = trained, 
      num = num,
      options = options,
      res = res,
      prefix = prefix
    )
  )
}

step_pca_new <- function(terms = NULL, 
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL, 
                         options = NULL,
                         res = NULL,
                         prefix = "PC") {
  
  step(
    subclass = "pca",
    terms = terms,
    role = role,
    trained = trained, 
    num = num,
    options = options,
    res = res,
    prefix = prefix
  )
}

#' @importFrom dimRed PCA dimRedData
learn.step_pca <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  x$num <- min(x$num, length(col_names))
  
  prc <- PCA(stdpars = x$options)
  
  prc <- prc@fun(dimRedData(as.data.frame(training[, col_names, drop = FALSE])), 
                 list(ndim = x$num))
  
  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    options = x$options,
    res = prc,
    prefix = x$prefix
  )
}

process.step_pca <- function(object, newdata, ...) {
  pca_vars <- rownames(environment(object$res@apply)$rot)
  comps <- object$res@apply(dimRedData(as.data.frame(newdata[, pca_vars, drop = FALSE])))@data
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- cbind(newdata, comps)
  newdata <- newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_pca <- function(x, width = 30, ...) {
  cat("PCA extraction with ")
  cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
