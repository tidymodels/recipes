## Buckets variables into discrete, mutally exclusive types
get_types <- function(x) {
  res <- lapply(x, get_types_recipes)
  tibble(variable = names(res), type = unname(res))
}

#' Get types for use in recipes
#'
#' The `get_types_recipes()` generic is used internally to supply types to
#' columns used in recipes. These functions underlie the work that the user sees
#' in [selections].
#'
#' This function acts as an extended recipes-specific version of [class()]. By
#' ignoring differences in similar types ("double" and "numeric") and allowing
#' each element to have multiple types ("factor" returns "factor", "unordered",
#' and "nominal", and "character" returns "string", "unordered", and
#' "nominal") we are able to create more natural selectors such as
#' [all_nominal()], [all_string()] and [all_integer()].
#'
#' @export
#' @keywords internal
#' @param x An object
#' @examplesIf rlang::is_installed("modeldata")
#'
#' data(Sacramento, package = "modeldata")
#' lapply(Sacramento, get_types_recipes)
get_types_recipes <- function(x) {
  UseMethod("get_types_recipes")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.default <- function(x) {
  "other"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.character <- function(x) {
  c("string", "unordered", "nominal")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.ordered <- function(x) {
  c("ordered", "nominal")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.factor <- function(x) {
  c("factor", "unordered", "nominal")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.integer <- function(x) {
  c("integer", "numeric")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.numeric <- function(x) {
  c("double", "numeric")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.double <- function(x) {
  c("double", "numeric")
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.Surv <- function(x) {
  "surv"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.logical <- function(x) {
  "logical"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.Date <- function(x) {
  "date"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.POSIXct <- function(x) {
  "datetime"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.list <- function(x) {
  "list"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.textrecipes_tokenlist <- function(x) {
  "tokenlist"
}

#' @export
#' @rdname get_types_recipes
get_types_recipes.hardhat_case_weights <- function(x) {
  "case_weights"
}
