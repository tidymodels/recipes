
#' @importFrom Matrix sparse.model.matrix
#' @importFrom stats model.matrix
convert_matrix <- function(x, sparse = TRUE) {
  is_num <- vapply(x, is.numeric, logical(1))
  if (!all(is_num)) {
    num_viol <- sum(!is_num)
    if (num_viol < 5)
      stop(
        "Columns (",
        paste0("`", names(is_num)[!is_num], "`", collapse = ", "),
        ") are not numeric; cannot convert to matrix.",
        call. = FALSE
      )
    else
      stop(num_viol, " columns are not numeric; cannot ",
           "convert to matrix.",
           call. = FALSE)
  }
  if (sparse)
    res <- sparse.model.matrix( ~ . + 0, data = x)
  else
    res <- model.matrix( ~ . + 0, data = x)
  res
}

