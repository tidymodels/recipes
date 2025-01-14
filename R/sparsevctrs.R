#' Using sparse data with recipes
#' 
#' [recipe()], [prep()], and [bake()] all accept sparse tibbles from the 
#' `sparsevctrs` package and sparse matrices from the `Matrix` package. Sparse
#' matrices are converted to sparse tibbles internally as each step expects a 
#' tibble as its input, and is expected to return a tibble as well.
#' 
#' Several steps work with sparse data. A step can either work with  sparse 
#' data, ruin sparsity, or create sparsity. The documentation for each  step 
#' will indicate whether it will work with sparse data or create sparse columns. 
#' If nothing is listed it is assumed to ruin sparsity.
#' 
#' Sparse tibbles or `data.frame`s will be returned from [bake()] if sparse columns 
#' are present in data, either from being generated in steps or because sparse 
#' data was passed into [recipe()], [prep()], or [bake()].
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