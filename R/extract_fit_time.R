#' @export
extract_fit_time.recipe <- function(x, summarize = TRUE, ...) {
  res <- x$fit_times

  if (summarize) {
    res <- tibble(
      id = "recipe",
      time = sum(res$time)
    )
  }

  res
}
