#' @section Sparse data:
#'
#' This step produces sparse columns if `sparse = "yes"` is being set. The
#' default value `"auto"` won't trigger production fo sparse columns if a recipe
#' is [prep()]ed, but allows for a workflow to toggle to `"yes"` or `"no"`
#' depending on whether the model supports [sparse_data] and if the model is
#' is expected to run faster with the data.
#'
#' The mechanism for determining how much sparsity is produced isn't perfect,
#' and there will be times when you want to manually overwrite by setting
#' `sparse = "yes"` or `sparse = "no"`.
