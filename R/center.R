#' Centering Numeric Data
#'
#' \code{step_center} creates a \emph{specification} of a recipe step that 
#'   will normalize numeric data to have a mean of zero.
#'
#' @param recipe A recipe object. The step will be added to the sequence of 
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are 
#'   affected by the step. See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing 
#'   have been estimated.
#' @param means A named numeric vector of means. This is \code{NULL} until 
#'   computed by \code{\link{prep.recipe}}.
#' @param na.rm A logical value indicating whether \code{NA} values should be 
#'   removed when averaging.
#' @return An updated version of \code{recipe} with the
#'   new step added to the sequence of existing steps (if any). 
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details Centering data means that the average of a variable is subtracted 
#'   from the data. \code{step_center} estimates the variable means from the 
#'   data used in the \code{training} argument of \code{prep.recipe}. 
#'   \code{bake.recipe} then applies the centering to new data sets using 
#'   these means.
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' center_trans <- rec %>%
#'   step_center(carbon, contains("gen"), -hydrogen)
#'
#' center_obj <- prep(center_trans, training = biomass_tr)
#'
#' transformed_te <- bake(center_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' @seealso \code{\link{recipe}} \code{\link{prep.recipe}} 
#'   \code{\link{bake.recipe}}
step_center <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           na.rm = TRUE) {
    add_step(
      recipe,
      step_center_new(
        terms = check_ellipses(...),
        trained = trained,
        role = role,
        means = means,
        na.rm = na.rm
      )
    )
  }

## Initializes a new object
step_center_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           means = NULL,
           na.rm = NULL) {
    step(
      subclass = "center",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      na.rm = na.rm
    )
  }

prep.step_center <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  means <-
    vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na.rm)
  step_center_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    na.rm = x$na.rm
  )
}

bake.step_center <- function(object, newdata, ...) {
  res <-
    sweep(as.matrix(newdata[, names(object$means)]), 2, object$means, "-")
  if (is.matrix(res) && ncol(res) == 1)
    res <- res[, 1]
  newdata[, names(object$means)] <- res
  as_tibble(newdata)
}

print.step_center <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Centering for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }
