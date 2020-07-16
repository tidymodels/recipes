
convert_matrix <- function(x, sparse = TRUE) {
  RsparseList_ind <- vapply(x, is_RsparseList, logical(1))

  x_RsparseList <- x[RsparseList_ind]
  x <- x[!RsparseList_ind]

  is_num <- vapply(x, is.numeric, logical(1))

  if (!all(is_num)) {
    num_viol <- sum(!is_num)
    if (num_viol < 5)
      rlang::abort(
        paste0(
          "Columns (",
          paste0("`", names(is_num)[!is_num], "`", collapse = ", "),
          ") are not numeric; cannot convert to matrix."
        )
      )
    else
      rlang::abort(
        paste0(
          num_viol,
          " columns are not numeric; cannot ",
          "convert to matrix."
        )
      )
  }

  # Issue-206: Don't use model.matrix(~ . + 0) here as it drops NA rows unless
  # na.pass is set. At this point, all cols are numeric so we can just use
  # as.matrix() (no need to worry about factor -> character conversion)

  res <- as.matrix(x)

  if (sparse) {
    res <- Matrix(res, sparse = TRUE)
  }

  if (length(x_RsparseList) > 0) {
    sparse_matrices <- purrr::map(x_RsparseList, RsparseList_to_CsparseMatrix)
    sparse_matrices <- purrr::reduce(sparse_matrices, cbind)

    res <- cbind(res, sparse_matrices)

    if (!sparse) {
      res <- as.matrix(res)
    }
  }

  res
}

is_RsparseList <- function(x) {
  !is.null(attr(x, "RsparseList"))
}

#' Convert sparse matric to RsparseList
#'
#' @param x A sparse matrix
#' @return A list of RsparseMatrix, one for each row in x.
#' @export
#' @importFrom methods as
#' @keywords internal
#' @rdname recipes-internal
CsparseMatrix_to_RsparseList <- function(x) {
  x <- methods::as(x, "RsparseMatrix")
  res <- purrr::map(seq_len(nrow(x)), ~x[.x,, drop = FALSE])
  attr(res, "RsparseList") <- TRUE
  res
}

RsparseList_to_CsparseMatrix <- function(x) {
  purrr::reduce(x, rbind)
}
