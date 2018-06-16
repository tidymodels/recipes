# valid classes for model.matrix convert_matrix
is_mm_class <- function(x) {
  is.numeric(x) | is.logical(x) | is.factor(x)
}

#' @importFrom Matrix sparse.model.matrix
#' @importFrom stats model.matrix
#' @importFrom purrr map_chr map_lgl
convert_matrix <- function(x, sparse = TRUE, type = "numeric") {
  if (type == "numeric") {
    ok_cols <- map_lgl(x, is_mm_class)
    num_viol <- sum(!ok_cols)
    if (num_viol) {
      if (num_viol < 5) {
        stop("All columns must be numeric, logical, or factors. ",
             "These columns are not: ",
             paste0("`", names(ok_cols)[!ok_cols], "`",
                    collapse = ","), ".", call. = FALSE)
      } else {
        stop("All columns must be numeric, logical, or factors. ",
             num_viol, " columns are not.", call. = FALSE)
      }
    }
    if (sparse) {
      res <- sparse.model.matrix(~ 0 + ., data = x)
    } else {
      res <- model.matrix(~ 0 + ., data = x)
    }
  } else if (type == "character") {
    res <- flatten_chr(map(x, as.character))
    dim(res) <- dim(x)
  }
  res


}

