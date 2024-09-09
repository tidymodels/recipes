is_sparse_tibble <- function(x) {
  any(vapply(x, sparsevctrs::is_sparse_vector, logical(1)))
}
