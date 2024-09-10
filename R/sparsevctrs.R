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
#' Spare tibbles or data.frames will be return from [bake()] if sparse columns 
#' are present in data, either from being generated in steps or because sparse 
#' data was passed into [recipe()], [prep()], or [bake()].
#' 
#' @name sparse_data
NULL

is_sparse_tibble <- function(x) {
  any(vapply(x, sparsevctrs::is_sparse_vector, logical(1)))
}

is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}
