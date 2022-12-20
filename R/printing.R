#' Printing Workhorse Function
#'
#' This internal function is used for printing steps.
#'
#' @param tr_obj A character vector of names that have been
#'  resolved during preparing the recipe (e.g. the `columns` object
#'  of [step_log()]).
#' @param untr_obj An object of selectors prior to prepping the
#'  recipe (e.g. `terms` in most steps).
#' @param trained A logical for whether the step has been trained.
#' @param title A character, shortly describing the action the step takes.
#' @param width An integer denoting where the output should be wrapped.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
#' @rdname recipes-internal
print_step <- function(tr_obj = NULL,
                       untr_obj = NULL,
                       trained = FALSE,
                       title = NULL,
                       width = max(20, options()$width - 30),
                       case_weights = NULL) {

  trained_text <- if_else(trained, "Trained", "")
  case_weights_text <- case_when(
    is.null(case_weights) ~ "",
    isTRUE(case_weights) ~ "weighted",
    isFALSE(case_weights) ~ "ignored weights"
  )

  vline_seperator <- if_else(trained_text == "", "", "|")
  comma_seperator <- if_else(
    trained_text != "" && case_weights_text != "",
    true = ",", false = ""
  )

  cli::cli_text(
    "{title} \\
    {vline_seperator} \\
    {.emph {trained_text}}\\
    {comma_seperator} \\
    {.emph {case_weights_text}}"
  )

  if (trained) {
    txt <- format_ch_vec(tr_obj, width = cli::console_width() - 3)
  } else {
    txt <- format_selectors(untr_obj, width = cli::console_width() - 3)
  }

  if (length(txt) == 0L) {
    txt <- "<none>"
  }

  cli::cli_li(txt)
  invisible(NULL)
}
