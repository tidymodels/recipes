#' Helpers for printing step functions
#'
#' @param x A vector of objects.
#' @param sep A character string for separating values.
#' @param width An integer for when to split the output over lines.
#' @return A character string
#' @keywords internal
#' @export
format_ch_vec <-
  function(x,
           sep = ", ",
           width = options()$width - 9) {
    widths <- nchar(x)
    sep_wd <- nchar(sep)
    adj_wd <- widths + sep_wd
    if (sum(adj_wd) >= width) {
      keepers <- max(which(cumsum(adj_wd) < width)) - 1
      if (length(keepers) == 0 || keepers < 1) {
        x <- paste(length(x), "items")
      } else {
        x <- c(x[1:keepers], "...")
      }
    }
    paste0(x, collapse = sep)
  }



#' @keywords internal
#' @rdname format_ch_vec
#' @export
format_selectors <- function(x, width = options()$width - 9) {
  ## convert to character without the leading ~
  x_items <- lapply(x, function(x) {
    expr_deparse(quo_get_expr(x))
  })

  x_items <- unlist(x_items)
  format_ch_vec(x_items, width = width, sep = ", ")
}
