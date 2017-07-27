#' General Variable Filter
#'
#' \code{step_rm} creates a \emph{specification} of a recipe step that will
#'   remove variables based on their name, type, or role.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables that
#'   will evaluated by the filtering bake. See \code{\link{selections}} for
#'   more details.
#' @param role Not used by this step since no new variables are created.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until
#'   \code{\link{prep.recipe}} is called.
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
#' smaller_set <- prep(smaller_set, training = biomass_tr)
#'
#' filtered_te <- bake(smaller_set, biomass_te)
#' filtered_te

step_rm <- function(recipe,
                    ...,
                    role = NA,
                    trained = FALSE,
                    removals = NULL) {
  add_step(recipe,
           step_rm_new(
             terms = check_ellipses(...),
             role = role,
             trained = trained,
             removals = removals
           ))
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
prep.step_rm <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_rm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = col_names
  )
}

#' @export
bake.step_rm <- function(object, newdata, ...) {
  if (length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_rm <-
  function(x, width = max(20, options()$width - 22), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Variables removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("No variables were removed")
    } else {
      cat("Delete terms ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
