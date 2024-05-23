sparse_make_dummies <- function(x, levels) {
  len <- length(x)

  out <- list()

  for (i in seq_along(levels)) {
    level <- levels[i]

    matches <- which(x == level)
    out[[level]] <- sparsevctrs::new_sparse_real(
      value = rep(1, length(matches)),
      position = matches,
      length = len
    )
  }

  dplyr::bind_cols(out)
}
