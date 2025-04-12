#' Using sparse data with recipes
#'
#' [recipe()], [prep()], and [bake()] all accept sparse tibbles from the
#' `sparsevctrs` package and sparse matrices from the `Matrix` package. Sparse
#' matrices are converted to sparse tibbles internally as each step expects a
#' tibble as its input, and is expected to return a tibble as well.
#'
#' Several steps work with sparse data. A step can either work with sparse data,
#' ruin sparsity, or create sparsity. The documentation for each step will
#' indicate whether it will work with sparse data or create sparse columns. If
#' nothing is listed it is assumed to ruin sparsity.
#'
#' Sparse tibbles or `data.frame`s will be returned from [bake()] if sparse
#' columns are present in data, either from being generated in steps or because
#' sparse data was passed into [recipe()], [prep()], or [bake()].
#'
#' @name sparse_data
NULL

is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}

#' Toggle all auto sparse arguments
#'
#' @param x A recipe.
#' @param choice A character string for separating values.
#'
#' @details
#'
#' If a step has an argument `sparse = "auto"`, then workflows can use this
#' function to fill these values with the preferred action. This preferred
#' action is calculated in workflows dependent on the model and data heuristics.
#' Hence why it has to be passed in.
#'
#' Only arguments where `sparse = "auto"` are affected, thus a user can set
#' `sparse = "no"` and it will be respected.
#'
#' @return A recipe
#'
#' @keywords internal
#'
#' @export
.recipes_toggle_sparse_args <- function(x, choice) {
  for (i in seq_along(x$steps)) {
    if (!is.null(x$steps[[i]]$sparse) && x$steps[[i]]$sparse == "auto") {
      x$steps[[i]]$sparse <- choice
    }
  }
  x
}

#' Estimate sparsity of a recipe
#'
#' @param x An object.
#'
#' @details
#'
#' Takes a untrained recipe an provides a rough estimate of the sparsity of the
#' prepped version of the recipe.
#'
#' Sampling of the input is done to avoid slowdown for larger data sets.
#'
#' An estimated sparsity of the input data is calculated. Then each step where
#' `sparse = "auto"` or `sparse = "yes"` is set, an estimate of how many
#' predictors will be created and used to modify the estimate.
#'
#' An initial sparsity of 0 will be used if a zero-row data frame is used in
#' specification of recipe. This is likely a under-estimate of the true sparsity
#' of the data.
#'
#' @return A recipe
#'
#' @keywords internal
#'
#' @export
.recipes_estimate_sparsity <- function(x, ...) {
  UseMethod(".recipes_estimate_sparsity")
}

#' @export
.recipes_estimate_sparsity.default <- function(x, ...) {
  NULL
}

#' @export
.recipes_estimate_sparsity.recipe <- function(x, ...) {
  template <- x$template
  n_rows <- nrow(template)
  n_cols <- ncol(template)

  if (n_rows == 0) {
    est_sparsity <- 0
    n_rows <- 1 # messed the math up otherwise
  } else {
    est_sparsity <- sparsevctrs::sparsity(template, sample = 1000)
  }
  zeroes <- est_sparsity * n_rows * n_cols

  for (step in x$steps) {
    if (!is.null(step$sparse) && step$sparse != "no") {
      col_names <- recipes_eval_select(
        step$terms,
        template,
        x$term_info,
        strict = FALSE
      )

      adjustments <- .recipes_estimate_sparsity(step, template[col_names])

      for (adjustment in adjustments) {
        zeroes <- zeroes +
          n_rows * adjustment[["sparsity"]] * adjustment[["n_cols"]]
        n_cols <- n_cols + adjustment[["n_cols"]] - 1
      }
    }
  }

  zeroes / (n_rows * n_cols)
}

check_sparse_arg <- function(x) {
  if (!is.null(x)) {
    rlang::arg_match0(x, c("auto", "yes", "no"), arg_nm = "sparse")
  }
}

sparse_is_yes <- function(x) {
  !is.null(x) && x == "yes"
}

#' Does step destroy sparsity of columns
#'
#' @param x An object.
#'
#' @details
#'
#' This function return `TRUE` if modifications by this step is expected to
#' destroy sparsity of its columns. It does not know whether it will happen or
#' not since it doesn't know if the step will select sparse columns or not.
#'
#' It returns `FALSE` for steps that have been modified to handle sparse columns
#' correctly.
#'
#' @return Single logical.
#'
#' @keywords internal
#'
#' @export
.recipes_preserve_sparsity <- function(x, ...) {
  UseMethod(".recipes_preserve_sparsity")
}

#' @export
.recipes_preserve_sparsity.default <- function(x, ...) {
  if (!inherits(x, "step")) {
    cli::cli_abort(
      "{.fn .recipes_preserve_sparsity} is only applicable to steps,
      not {.obj_type_friendly {x}}."
    )
  }

  FALSE
}
