#' General Variable Filter
#'
#' `step_rm` creates a *specification* of a recipe step
#'  that will remove variables based on their name, type, or role.
#'
#' @inheritParams step_center
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until [prep.recipe()] is called.
#' @template step-return
#' @details When you [`tidy()`] this step, a tibble with column `terms` (the
#'  columns that will be removed) is returned.
#' @family variable filter steps
#' @export
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' library(dplyr)
#' smaller_set <- rec %>%
#'   step_rm(contains("gen"))
#'
#' smaller_set <- prep(smaller_set, training = biomass_tr)
#'
#' filtered_te <- bake(smaller_set, biomass_te)
#' filtered_te
#'
#' tidy(smaller_set, number = 1)
step_rm <- function(recipe,
                    ...,
                    role = NA,
                    trained = FALSE,
                    removals = NULL,
                    skip = FALSE,
                    id = rand_id("rm")) {
  add_step(
    recipe,
    step_rm_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      removals = removals,
      skip = skip,
      id = id
    )
  )
}

step_rm_new <- function(terms, role, trained, removals, skip, id) {
  step(
    subclass = "rm",
    terms = terms,
    role = role,
    trained = trained,
    removals = removals,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_rm <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  step_rm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_rm <- function(object, new_data, ...) {
  if (length(object$removals) > 0) {
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  }
  as_tibble(new_data)
}

print.step_rm <-
  function(x, width = max(20, options()$width - 22), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Variables removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else {
        cat("No variables were removed")
      }
    } else {
      cat("Delete terms ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_rm <- tidy_filter
