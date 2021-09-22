#' Zero Variance Filter
#'
#' `step_zv` creates a *specification* of a recipe step
#'  that will remove variables that contain only a single value.
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
#'
#' @examples
#' library(modeldata)
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
#'
#' tidy(zv_filter, number = 1)
#' tidy(filter_obj, number = 1)
step_zv <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           removals = NULL,
           skip = FALSE,
           id = rand_id("zv")) {
    add_step(
      recipe,
      step_zv_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        removals = removals,
        skip = skip,
        id = id
      )
    )
  }

step_zv_new <-
  function(terms, role, trained, removals, skip, id) {
    step(
      subclass = "zv",
      terms = terms,
      role = role,
      trained = trained,
      removals = removals,
      skip = skip,
      id = id
    )
  }

one_unique <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) < 2
  }

#' @export
prep.step_zv <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  filter <- vapply(training[, col_names], one_unique, logical(1))

  step_zv_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = names(filter)[filter],
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_zv <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
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
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_zv <- tidy_filter
