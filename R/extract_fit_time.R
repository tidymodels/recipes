#' @export
extract_fit_time.recipe <- function(x, summarize = TRUE, ...) {
  res <- x$fit_times

  if (is.null(res)) {
    cli::cli_abort(
      "This recipe was created before {.fn recipes::extract_fit_time} was \\
      added. Fit time cannot be extracted."
    )
  }

  if (identical(res, tibble::tibble())) {
    res <- tibble(stage_id = character(), elapsed = numeric())
  }

  if (summarize) {
    res <- tibble(
      stage_id = "recipe",
      elapsed = sum(res$elapsed)
    )
  }

  res
}
