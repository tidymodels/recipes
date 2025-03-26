#' Helpers for printing step functions
#'
#' @param x A vector of objects.
#' @param sep A character string for separating values.
#' @param width An integer for when to split the output over lines.
#' @return A character string
#' @keywords internal
#' @export
format_ch_vec <-
  function(x, sep = ", ", width = options()$width - 9) {
    as.character(glue::glue_collapse(x, sep = sep, width = width))
  }

#' @keywords internal
#' @rdname format_ch_vec
#' @export
format_selectors <- function(x, width = options()$width - 9) {
  ## convert to character without the leading ~
  x_items <- lapply(x, function(x) {
    expr_deparse(quo_get_expr(x), width = Inf)
  })

  x_items <- vctrs::list_unchop(x_items, ptype = character())
  format_ch_vec(x_items, width = width, sep = ", ")
}
