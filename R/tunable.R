#' @export
tunable.recipe <- function(x, ...) {
  if (length(x$steps) == 0) {
    res <- no_param
  } else {
    res <- purrr::map_dfr(x$steps, tunable)
    if (nrow(res) > 0) {
      res <- res[!is.na(res$name),]
    }
  }
  res
}

#' @export
tunable.step <- function(x, ...) {
  no_param
}

step_type <- function(.step) class(.step)[class(.step) != "step"][1]

no_param <-
  tibble::tibble(
    name = NA_character_,
    call_info = list(),
    source = NA_character_,
    component = NA_character_,
    component_id = NA_character_
  )

#' @export
tunable.check <- function(x, ...) {
  no_param
}
