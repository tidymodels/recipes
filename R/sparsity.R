
convert_matrix <- function(x, sparse = TRUE) {
  is_list <- vapply(x, is.list, logical(1))

  x_lists <- x[is_list]
  x <- x[!is_list]

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

  if (length(x_lists) > 0) {
    sparse_matrices <- purrr::map(x_lists, purrr::reduce, rbind)
    sparse_matrices <- purrr::reduce(sparse_matrices, cbind)

    res <- cbind(res, sparse_matrices)

    if (!sparse) {
      res <- as.matrix(res)
    }
  }

  res
}
