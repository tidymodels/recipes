#' General Variable Filter
#' 
#' \code{step_rm} creates a \emph{specification} of a recipe step that will remove variables based on their name, type, or role. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will evaluated by the filtering process.
#' @param role Not used by this step since no new variables are created.
#' @param removals A character string that contains the names of columns that should be removed. These values are not determined until \code{\link{learn.recipe}} is called. 
#' @return \code{step_rm}  returns an object of class \code{step_rm}.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#' @examples 
#' data(biomass)
#' 
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#' 
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#' 
#' library(dplyr)
#' smaller_set <- rec %>%
#'   step_rm(terms = ~ contains("gen")) 
#' 
#' smaller_set <- learn(smaller_set, training = biomass_tr)
#' 
#' filtered_te <- process(smaller_set, biomass_te, role = "predictor")
#' filtered_te

step_rm <- function(recipe, 
                      terms, 
                      role = NA,
                      trained = FALSE,
                      removals = NULL) {
  add_step(
    recipe, 
    step_rm_new(
      terms = terms, 
      role = role,
      trained = trained,
      removals = removals
    )
  )
}

step_rm_new <- function(terms = NULL, 
                          role = NA,
                          trained = FALSE,
                          removals = NULL) {
  step(
    subclass = "rm", 
    terms = terms,
    role = role,
    trained = trained,
    removals = removals
  )
}

learn.step_rm <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 

  step_rm_new(
    terms = x$terms, 
    role = x$role,
    trained = TRUE,
    removals = col_names
  )
}

process.step_rm <- function(object, newdata, ...) {
  if(length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_rm <- function(x, width = 30, ...) {
  cat("Delete terms ")
  cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
