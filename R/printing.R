#' Printing workhorse function
#'
#' `print_step()` is used for printing steps.
#'
#' @param tr_obj A character vector of names that have been resolved during
#'   preparing the recipe (e.g. the `columns` object of [step_log()]).
#' @param untr_obj An object of selectors prior to prepping the recipe (e.g.
#'   `terms` in most steps).
#' @param trained A logical for whether the step has been trained.
#' @param title A character, shortly describing the action the step takes.
#' @param width An integer denoting where the output should be wrapped.
#'
#' @return `print_step()`: `NULL`, invisibly.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @rdname recipes-internal
#' @export
print_step <- function(
  tr_obj = NULL,
  untr_obj = NULL,
  trained = FALSE,
  title = NULL,
  width = max(20, options()$width - 30),
  case_weights = NULL
) {
  title <- trimws(title)

  trained_text <- dplyr::if_else(trained, "Trained", "")
  case_weights_text <- dplyr::case_when(
    is.null(case_weights) ~ "",
    isTRUE(case_weights) ~ "weighted",
    isFALSE(case_weights) ~ "ignored weights"
  )

  vline_seperator <- dplyr::if_else(trained_text == "", "", "|")
  comma_seperator <- dplyr::if_else(
    trained_text != "" && case_weights_text != "",
    true = ",",
    false = ""
  )

  width_title <- nchar(
    paste0(
      "* ",
      title,
      ":",
      " ",
      vline_seperator,
      " ",
      trained_text,
      " ",
      comma_seperator,
      " ",
      case_weights_text
    )
  )

  width_diff <- cli::console_width() * 1 - width_title

  if (trained) {
    elements <- tr_obj
  } else {
    elements <- lapply(untr_obj, function(x) {
      expr_deparse(quo_get_expr(x), width = Inf)
    })

    elements <- vctrs::list_unchop(elements, ptype = character())
  }

  if (length(elements) == 0L) {
    elements <- "<none>"
  }

  element_print_lengths <- cumsum(nchar(elements)) + # length of elements
    c(0L, cumsum(rep(2L, length(elements) - 1))) + # length of comma seperator
    c(rep(5L, length(elements) - 1), 0L) + # length of `, ...`
    3 # to account for possible " and " instead of ", "

  first_line <- which(width_diff >= element_print_lengths)
  first_line <- unname(first_line)
  first_line <- ifelse(
    test = identical(first_line, integer(0)),
    yes = length(element_print_lengths),
    no = max(first_line)
  )

  more_dots <- ifelse(first_line == length(elements), "", ", ...")

  cli::cli_bullets(
    c(
      "*" = "
    {title}: \\
    {.pkg {elements[seq_len(first_line)]}}\\
    {more_dots} \\
    {vline_seperator} \\
    {.emph {trained_text}}\\
    {comma_seperator} \\
    {.emph {case_weights_text}}
    "
    )
  )

  invisible(NULL)
}
