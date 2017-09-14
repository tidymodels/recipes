#' Zero Variance Filter
#'
#' \code{step_zv} creates a \emph{specification} of a recipe step
#'  that will remove variables that contain only a single value.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables that will evaluated by the filtering. See
#'  \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until \code{\link{prep.recipe}} is called.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#'
#' @examples
#' data(biomass)
#'
#' biomass$one_value <- 1
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen +
#'                     nitrogen + sulfur + one_value,
#'               data = biomass_tr)
#'
#' zv_filter <- rec %>%
#'   step_zv(all_predictors())
#'
#' filter_obj <- prep(zv_filter, training = biomass_tr)
#'
#' filtered_te <- bake(filter_obj, biomass_te)
#' any(names(filtered_te) == "one_value")
#' @seealso \code{\link{step_nzv}} \code{\link{step_corr}}
#'   \code{\link{recipe}}
#'   \code{\link{prep.recipe}} \code{\link{bake.recipe}}

step_zv <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           removals = NULL) {
    add_step(
      recipe,
      step_zv_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        removals = removals
      )
    )
  }

step_zv_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           removals = NULL) {
    step(
      subclass = "zv",
      terms = terms,
      role = role,
      trained = trained,
      removals = removals
    )
  }

one_unique <- function(x)
  length(unique(x)) == 1

#' @export
prep.step_zv <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  filter <- vapply(training[, col_names], one_unique, logical(1))

  step_zv_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = names(filter)[filter]
  )
}

#' @export
bake.step_zv <- function(object, newdata, ...) {
  if (length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_zv <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Zero variance filter removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("Zero variance filter removed no terms")
    } else {
      cat("Zero variance filter on ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
