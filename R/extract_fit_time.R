#' @export
extract_fit_time.recipe <- function(x, summarize = TRUE, ...) {
  res <- x$fit_times

  if (is.null(res)) {
    rlang::abort(
      "This recipe was created before `extract_fit_time()` was added."
    )
  }

  if (summarize) {
    res <- tibble(
      process_id = "recipe",
      time = sum(res$time)
    )
  }

  res
}
