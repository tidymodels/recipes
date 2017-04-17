#' General Variable Filter
#'
#' \code{step_rm} creates a \emph{specification} of a recipe step that will remove variables based on their name, type, or role.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables that will evaluated by the filtering process. See \code{\link{selections}} for more details.
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
#'   step_rm(contains("gen"))
#'
#' smaller_set <- learn(smaller_set, training = biomass_tr)
#'
#' filtered_te <- process(smaller_set, biomass_te)
#' filtered_te

step_rm <- function(recipe,
                    ...,
                    role = NA,
                    trained = FALSE,
                    removals = NULL) {
  terms <- dots_quos(...)
  if(is_empty(terms))
    stop("Please supply at least one variable specification. See ?selections.")
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

#' @export
learn.step_rm <- function(x, training, info = NULL, ...) {
  col_names <- select_terms(x$terms, info = info)

  step_rm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = col_names
  )
}

#' @export
process.step_rm <- function(object, newdata, ...) {
  if(length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_rm <- function(x, width = max(20, options()$width - 22), ...) {
  if(x$trained) {
    cat("Removed ")
    cat(format_ch_vec(x$removals, width = width))
  } else {
    cat("Delete terms ", sep = "")
    cat(format_formula(x$terms, wdth = width))
  }
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
