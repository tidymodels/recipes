#' Zero variance filter
#'
#' `step_zv()` creates a *specification* of a recipe step that will remove
#' variables that contain only a single value.
#'
#' @inheritParams step_center
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @param group An optional character string or call to [dplyr::vars()] that can
#'   be used to specify a group(s) within which to identify variables that
#'   contain only a single value. If the grouping variables are contained in
#'   terms selector, they will not be considered for removal.
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
#'   \item{terms}{character, names of the columns that will be removed}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-not-supported
#'
#' @family variable filter steps
#' @export
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass$one_value <- 1
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen +
#'   nitrogen + sulfur + one_value,
#' data = biomass_tr
#' )
#'
#' zv_filter <- rec |>
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
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    group = NULL,
    removals = NULL,
    skip = FALSE,
    id = rand_id("zv")
  ) {
    add_step(
      recipe,
      step_zv_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        group = group,
        removals = removals,
        skip = skip,
        id = id
      )
    )
  }

step_zv_new <-
  function(terms, role, trained, group, removals, skip, id) {
    step(
      subclass = "zv",
      terms = terms,
      role = role,
      trained = trained,
      group = group,
      removals = removals,
      skip = skip,
      id = id
    )
  }

one_unique <- function(x) {
  x <- x[!is.na(x)]
  if (sparsevctrs::is_sparse_vector(x)) {
    res <- length(unique(sparsevctrs::sparse_values(x))) == 0
  } else {
    res <- length(unique(x)) < 2
  }
  res
}

group_one_unique <- function(x, f) {
  x_split <- split(x, f)
  any(vapply(x_split, one_unique, logical(1)))
}

#' @export
prep.step_zv <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  group_names <- recipes_eval_select(x$group, training, info)

  if (is.null(x$group)) {
    filter <- vapply(training[, col_names], one_unique, logical(1))
  } else {
    filter <- vapply(
      training[, setdiff(col_names, group_names)],
      group_one_unique,
      f = interaction(training[group_names]),
      logical(1)
    )
  }

  step_zv_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    group = x$group,
    removals = names(filter)[filter],
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_zv <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_zv <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      title <- "Zero variance filter removed "
    } else {
      title <- "Zero variance filter on "
    }
    print_step(x$removals, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_zv <- tidy_filter

#' @export
.recipes_preserve_sparsity.step_zv <- function(x, ...) {
  TRUE
}
