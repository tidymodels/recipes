
#' @importFrom Matrix Matrix
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

  # Issue-206: Don't use model.matrix(~ . + 0) here as it drops NA rows unless
  # na.pass is set. At this point, all cols are numeric so we can just use
  # as.matrix() (no need to worry about factor -> character conversion)

  res <- as.matrix(x)

  if (sparse) {
    res <- Matrix(res, sparse = TRUE)
  }

  res
}




to_list_of_matrix <- function(col_group, x, is_df = FALSE) {

  if (is_df) {
    return(I(convert_matrix(select(x, col_group), sparse = FALSE)))
  } else {
    return(convert_matrix(select(x, col_group), sparse = FALSE))
  }

}



convert_tibble_of_matrices <- function(x, term_info) {

  role_group <- split(term_info$variable, term_info$role)

  as_tibble(lapply(role_group, to_list_of_matrix, x = x, is_df = FALSE))


}

convert_df_of_matrices <- function(x, term_info) {

  role_group <- split(term_info$variable, term_info$role)

  as.data.frame(lapply(role_group, to_list_of_matrix, x = x, is_df = TRUE))

}

