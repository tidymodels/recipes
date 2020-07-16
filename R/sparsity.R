
convert_matrix <- function(x, sparse = TRUE) {
  RsparseList_ind <- vapply(x, is_RsparseList, logical(1))

  if (!any(RsparseList_ind)) {
    return(x)
  }

  x_RsparseList <- x[RsparseList_ind]
  column_order <- insert_all(names(x), x_RsparseList)
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

  res[, column_order, drop = FALSE]
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

expand_RsparseList <- function(x, to_tibble = FALSE) {
  RsparseList_ind <- vapply(x, is_RsparseList, logical(1))

  if (!any(RsparseList_ind)) {
    return(x)
  }

  x_RsparseList <- x[RsparseList_ind]
  x_mat <- x[!RsparseList_ind]

  column_order <- insert_all(names(x), x_RsparseList)

  sparse_matrices <- purrr::map(x_RsparseList, RsparseList_to_CsparseMatrix)
  sparse_matrices <- purrr::reduce(sparse_matrices, cbind)
  sparse_matrices <- as.data.frame(as.matrix(sparse_matrices))

  if (to_tibble) {
    sparse_matrices <- as_tibble(sparse_matrices)
  }

  res <- bind_cols(sparse_matrices, x_mat)
  res[column_order]
}

insert_replace <- function(x, location, replacement) {
  split <- which(x == location)
  if (split == length(x)) {
    c(x[-split], replacement)
  } else {
    c(x[seq(0, split - 1)], replacement, x[seq(split + 1, length(x))])
  }
}

insert_all <- function(x, y) {
  locations <- names(y)
  replacements <- map(y, ~colnames(.x[[1]]))

  for (i in seq_along(y)) {
    x <- insert_replace(x, locations[i], replacements[[i]])
  }
  x
}


