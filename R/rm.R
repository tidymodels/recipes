#' General variable filter
#'
#' `step_rm()` creates a *specification* of a recipe step that will remove
#' selected variables.
#'
#' @inheritParams step_center
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @template step-return
#' @template filter-steps
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-not-supported
#'
#' @family variable filter steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' library(dplyr)
#' smaller_set <- rec |>
#'   step_rm(contains("gen"))
#'
#' smaller_set <- prep(smaller_set, training = biomass_tr)
#'
#' filtered_te <- bake(smaller_set, biomass_te)
#' filtered_te
#'
#' tidy(smaller_set, number = 1)
step_rm <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  removals = NULL,
  skip = FALSE,
  id = rand_id("rm")
) {
  add_step(
    recipe,
    step_rm_new(
      terms = enquos(...),
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
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_rm <-
  function(x, width = max(20, options()$width - 22), ...) {
    title <- "Variables removed "
    print_step(x$removals, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_rm <- tidy_filter

#' @export
.recipes_preserve_sparsity.step_rm <- function(x, ...) {
  TRUE
}
