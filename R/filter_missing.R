#' Missing Value Column Filter
#'
#' `step_filter_missing` creates a *specification* of a recipe
#'  step that will potentially remove variables that have too many missing
#'  values.
#'
#' @inheritParams step_center
#' @param threshold A value for the threshold of missing values in column. The
#'  step will remove the columns where the proportion of missing values
#'  exceeds the threshold.
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until [prep()] is called.
#' @template step-return
#' @template filter-steps
#' @family variable filter steps
#' @export
#'
#' @details This step will remove variables if the proportion of missing
#'  values exceeds the `threshold`.
#'
#' All variables with missing values will be removed for `threshold = 0`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#' `terms` (the columns that will be removed) is returned.
#'
#' @examples
#' library(modeldata)
#' data(credit_data)
#'
#' rec <- recipe(Status ~ ., data = credit_data) %>%
#'   step_filter_missing(all_predictors(), threshold = 0)
#'
#' filter_obj <- prep(rec)
#'
#' filtered_te <- bake(filter_obj, new_data = NULL)
#'
#' tidy(rec, number = 1)
#' tidy(filter_obj, number = 1)
step_filter_missing <- function(recipe,
                      ...,
                      role = NA,
                      trained = FALSE,
                      threshold = 0.1,
                      removals = NULL,
                      skip = FALSE,
                      id = rand_id("filter_missing")
) {
  add_step(
    recipe,
    step_filter_missing_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      threshold = threshold,
      removals = removals,
      skip = skip,
      id = id
    )
  )
}

step_filter_missing_new <-
  function(terms, role, trained, threshold, removals, skip, id) {
    step(
      subclass = "filter_missing",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      removals = removals,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_filter_missing <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (length(col_names) > 1) {
    filter <- filter_missing_fun(
      x = training[, col_names],
      threshold = x$threshold
    )
  } else {
    filter <- character(0)
  }

  step_filter_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    removals = filter,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_filter_missing <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

print.step_filter_missing <-
  function(x,  width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      title <- "Missing value column filter removed "
    } else {
      title <- "Missing value column filter on "
    }
    print_step(x$removals, x$terms, x$trained, title, width)
    invisible(x)
  }

filter_missing_fun <- function(x, threshold) {
  missing <- purrr::map_dbl(x, ~ mean(is.na(.x)))
  removal_ind <- which(missing > threshold)
  names(x)[removal_ind]
}

#' @rdname tidy.recipe
#' @export
tidy.step_filter_missing <- tidy_filter

#' @export
tunable.step_filter_missing <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0.05, 1.00))
    ),
    source = "recipe",
    component = "step_filter_missing",
    component_id = x$id
  )
}
