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
                       width = max(20, options()$width - 30)) {
  cat(title)

  if (trained) {
    txt <- format_ch_vec(tr_obj, width = width)
  } else {
    txt <- format_selectors(untr_obj, width = width)
  }

  if (length(txt) == 0L) {
    txt <- "<none>"
  }

  cat(txt)

  if (trained) {
    cat(" [trained]\n")
  } else {
    cat("\n")
  }

  invisible(NULL)
}