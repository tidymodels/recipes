#' @param x a recipe
#'
#' @rdname recipes_get_features
#' @export
#' @param x An object
#' @examplesIf rlang::is_installed("modeldata")
#'
#' data(Sacramento, package = "modeldata")
#' lapply(Sacramento, .get_data_types)
recipes_get_features_in <- function(x) {
  UseMethod("recipes_get_features_in")
}

#' @export
#' @rdname recipes_get_features
recipes_get_features_in.recipe <- function(x) {
  res <- map(x$steps, recipes_get_features_in)
  res <- purrr::list_rbind(res)
  res
}

#' @export
#' @rdname recipes_get_features
recipes_get_features_in.step <- function(x) {
  step_name <- class(x)[1]
  if (is.character(x$columns)) {
    res <- tibble(step = step_name, columns = unname(x$columns))
    return(res)
  }
  if (is.character(x$columns$input)) {
    res <- tibble(step = step_name, columns = x$columns$input)
    return(res)
  }

  abort("Can't access input variables names.")
}

#' @export
#' @rdname recipes_get_features
recipes_get_features_in.default <- function(x) {
  cli::cli_abort("Cannot be applied to object of class {class(x)[1]}.")
}


#' @rdname recipes_get_features
#' @export
recipes_get_features_out <- function(x) {
  UseMethod("recipes_get_features_out")
}

#' @export
#' @rdname recipes_get_features_out
recipes_get_features_out.recipe <- function(x) {
  "other"
}

#' @export
#' @rdname recipes_get_features_out
recipes_get_features_out.step <- function(x) {
  "other"
}

#' @export
#' @rdname recipes_get_features_out
recipes_get_features_out.default <- function(x) {
  "other"
}
