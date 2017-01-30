#' Create Interaction Variables
#' 
#' \code{step_interact} creates a \emph{specification} of a recipe step that will create new columns that are interaction terms between two or more variables.
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A traditional R formula that contains interaction terms.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new columns created from the original variables will be used as predictors in a model.  
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param objects A list of \code{terms} objects for each individual interation.
#' @param sep A character value used to delinate variables in an interaction (e.g. \code{var1_x_var2} instead of the more traditional \code{var1:var2}.
#' @return \code{step_interact} and \code{learn.interact_step} return objects of class \code{interact_step}.
#' @keywords datagen
#' @concept preprocessing 
#' @export
#' 
#' 
step_interact <- function(recipe, terms, role = "predictor", trained = FALSE, objects = NULL, sep = "_x_") {
  add_step(
    recipe, 
    step_interact_new(
      terms = terms, 
      trained = trained,
      role = role, 
      objects = objects,
      sep = sep))
}

## Initializes a new object
step_interact_new <- function(terms = NULL, role = NA, trained = FALSE, objects = NULL, sep = NULL) {
  step(
    subclass = "interact", 
    terms = terms,
    role = role,
    trained = trained,
    objects = objects,
    sep = sep
  )
}

#' For a training set of data, \code{learn.interact_step} computes the required information to produce interaction terms. Note that no interactions are created by this function.
#' 
#' @param x a \code{interact_step} object 
#' @param data a tibble or data frame that contains the training set. 
#' @param ... further arguments passed to or from other methods (not currently used).
#' @export
#' @importFrom stats sd
#' @rdname step_interact

## The idea is to save a bunch of x-factor interaction terms instead of 
## one large set of collected terms. 
learn.interact_step <- function(x, data, na.rm = TRUE, ...) {
  ## First, find the interaction terms based on the given formula
  int_terms <- get_term_names(x$terms, vnames = colnames(data))
  ## For each interaction, create a new formula that has main effects
  ## and only the interaction of choice (e.g. `a+b+c+a:b:c`)
  int_forms <- make_new_formula(int_terms)
  ## Generate a standard R `terms` object from these short formulas and 
  ## save to make future interactions
  int_terms <- make_small_terms(int_forms, data)
  step_interact_new(
    terms = x$terms, 
    role = x$role, 
    trained = TRUE, 
    objects = int_terms, 
    sep = x$sep
  )
}

#' \code{process.interact_step} augment the current data with columns containing the interactions. 
#' 
#' @param data A tibble or data frame that has numeric variables for the interactions.
#' @return \code{process.interact_step} returns a tibble of processed data. 
#' @export
#' @importFrom tibble as_tibble
#' @importFrom stats model.matrix
#' @rdname step_interact

process.interact_step <- function(x, data, ...) {
  ## Create low level model matrices then remove the non-interaction terms.
  res <- lapply(x$object, model.matrix, data = data)
  res <- lapply(res, function(x) x[, grepl(":", colnames(x)), drop = FALSE])
  ncols <- vapply(res, ncol, c(int = 1L))
  out <- matrix(NA, nrow = nrow(data), ncol = sum(ncols))
  strt <- 1
  for(i in seq_along(ncols)) {
    cols <- (strt):(strt+ncols[i]-1)
    out[, cols] <- res[[i]]
    strt <- max(cols)+1
  }
  colnames(out) <- gsub(":", x$sep, unlist(lapply(res, colnames)))
  data <- cbind(data, out)
  as_tibble(data)
}

## This uses the highest level of interactions
x_fac_int <- function(x) 
  as.formula(paste0("~", paste0(x, collapse = "+"), "+", paste0(x, collapse = ":")))

make_new_formula <- function(x){
  splitup <- strsplit(x, ":")
  lapply(splitup, x_fac_int)
}


#' @importFrom stats model.matrix

## Given a standard model formula and some data, get the 
## term expansion (without `.`s). This returns the factor
## names and would not expand dummy variables. 
get_term_names <- function(form, vnames) {
  ## We are going to cheat and make a small fake data set to 
  ## effcicently get the full formula exapnsion from 
  ## model.matrix (devoid of factor levels) and then
  ## pick off the interactions
  dat <- matrix(1, nrow = 5, ncol = length(vnames))
  colnames(dat) <- vnames
  nms <- colnames(model.matrix(form, data = as.data.frame(dat)))
  nms <- nms[nms != "(Intercept)"]
  nms <- grep(":", nms, value = TRUE)
  nms
}

#' @importFrom stats terms

## For a given data set and a list of formulas, generate the 
## standard R `terms`` objects
make_small_terms <- function(forms, dat) {
  lapply(forms, terms, data = dat)
}
