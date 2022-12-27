
convert_matrix <- function(x, sparse = TRUE) {
  # is_num <- vapply(x, is.numeric, logical(1))

  # if (!all(is_num)) {
  #   num_viol <- sum(!is_num)
  #   if (num_viol < 5) {
  #     rlang::abort(
  #       paste0(
  #         "Columns (",
  #         paste0("`", names(is_num)[!is_num], "`", collapse = ", "),
  #         ") are not numeric; cannot convert to matrix."
  #       )
  #     )
  #   } else {
  #     rlang::abort(
  #       paste0(
  #         num_viol,
  #         " columns are not numeric; cannot ",
  #         "convert to matrix."
  #       )
  #     )
  #   }
  # }

  # Issue-206: Don't use model.matrix(~ . + 0) here as it drops NA rows unless
  # na.pass is set. At this point, all cols are numeric so we can just use
  # as.matrix() (no need to worry about factor -> character conversion)

  # res <- as.matrix(x)
  if (sparse) {
  #  res <- Matrix(res, sparse = TRUE)
    all_positions <- lapply(x, function(x) attr(x, "positions"))

    i <- rep(seq_along(all_positions), lengths(all_positions))
    j <- unname(unlist(all_positions))

    res <- Matrix::sparseMatrix(
      j = i[!is.na(j)],
      i = j[!is.na(j)],
      dims = c(nrow(x), ncol(x)),
      x = 1
    )

    res@Dimnames[[2]] <- colnames(x)
  }

  res
}
