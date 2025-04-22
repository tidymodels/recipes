#' Missing value column filter
#'
#' `step_filter_missing()` creates a *specification* of a recipe step that will
#' potentially remove variables that have too many missing values.
#'
#' @inheritParams step_center
#' @param threshold A value for the threshold of missing values in column. The
#'   step will remove the columns where the proportion of missing values exceeds
#'   the threshold.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @template step-return
#' @template filter-steps
#' @family variable filter steps
#' @export
#'
#' @details
#'
#' This step will remove variables if the proportion of missing values exceeds
#' the `threshold`.
#'
#' All variables with missing values will be removed for `threshold = 0`.
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
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_filter_missing"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template sparse-preserve
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(credit_data, package = "modeldata")
#'
#' rec <- recipe(Status ~ ., data = credit_data) |>
#'   step_filter_missing(all_predictors(), threshold = 0)
#'
#' filter_obj <- prep(rec)
#'
#' filtered_te <- bake(filter_obj, new_data = NULL)
#'
#' tidy(rec, number = 1)
#' tidy(filter_obj, number = 1)
step_filter_missing <- function(
  recipe,
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
      id = id,
      case_weights = NULL
    )
  )
}

step_filter_missing_new <-
  function(terms, role, trained, threshold, removals, skip, id, case_weights) {
    step(
      subclass = "filter_missing",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_filter_missing <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_number_decimal(x$threshold, min = 0, max = 1, arg = "threshold")

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(col_names) > 1) {
    filter <- filter_missing_fun(
      x = training[, col_names],
      threshold = x$threshold,
      wts = wts
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
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_filter_missing <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_filter_missing <-
  function(x, width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      title <- "Missing value column filter removed "
    } else {
      title <- "Missing value column filter on "
    }
    print_step(
      x$removals,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

filter_missing_fun <- function(x, threshold, wts) {
  n <- NCOL(x)
  removal_ind <- logical(n)

  for (i in seq_len(n)) {
    values <- x[[i]]

    if (sparsevctrs::is_sparse_vector(values)) {
      nas <- sparsevctrs::sparse_is_na(values, type = "integer")
      missing <- sparsevctrs::sparse_mean(nas, wts = wts)
    } else {
      nas <- is.na(values)
      missing <- averages(data.frame(nas), wts = wts)
    }

    if (missing > threshold) {
      removal_ind[[i]] <- TRUE
    }
  }

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

#' @export
.recipes_preserve_sparsity.step_filter_missing <- function(x, ...) {
  TRUE
}
