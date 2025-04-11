#' @details
#'
#' When using this flexible step, use extra care to avoid data leakage in your
#' preprocessing. Consider, for example, the transformation `x = w > mean(w)`.
#' When applied to new data or testing data, this transformation would use the
#' mean of `w` from the _new_ data, not the mean of `w` from the training data.
