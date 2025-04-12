## Buckets variables into discrete, mutally exclusive types
get_types <- function(x) {
  res <- lapply(x, .get_data_types)
  tibble(variable = names(res), type = unname(res))
}

#' Get types for use in recipes
#'
#' The `.get_data_types()` generic is used internally to supply types to columns
#' used in recipes. These functions underlie the work that the user sees in
#' [selections].
#'
#' This function acts as an extended recipes-specific version of [class()]. By
#' ignoring differences in similar types ("double" and "numeric") and allowing
#' each element to have multiple types ("factor" returns "factor", "unordered",
#' and "nominal", and "character" returns "string", "unordered", and "nominal")
#' we are able to create more natural selectors such as [all_nominal()],
#' [all_string()] and [all_integer()].
#'
#' The following list shows the data types for different classes, as defined by
#' recipes. If an object has a class not supported by `.get_data_types()`, it
#' will get data type "`r all_get_data_types$default`".
#'
#' - character: `r all_get_data_types$character`
#' - ordered: `r all_get_data_types$ordered`
#' - factor: `r all_get_data_types$factor`
#' - integer: `r all_get_data_types$integer`
#' - numeric: `r all_get_data_types$numeric`
#' - double: `r all_get_data_types$double`
#' - Surv: `r all_get_data_types$Surv`
#' - logical: `r all_get_data_types$logical`
#' - Date: `r all_get_data_types$Date`
#' - POSIXct: `r all_get_data_types$POSIXct`
#' - list: `r all_get_data_types$list`
#' - textrecipes_tokenlist: `r all_get_data_types$textrecipes_tokenlist`
#' - hardhat_case_weights: `r all_get_data_types$hardhat_case_weights`
#'
#' @rdname get_data_types
#'
#' @seealso [developer_functions]
#'
#' @export
#' @param x An object
#' @examplesIf rlang::is_installed("modeldata")
#'
#' data(Sacramento, package = "modeldata")
#' lapply(Sacramento, .get_data_types)
.get_data_types <- function(x) {
  UseMethod(".get_data_types")
}

#' @export
#' @rdname get_data_types
.get_data_types.default <- function(x) {
  "other"
}

#' @export
#' @rdname get_data_types
.get_data_types.character <- function(x) {
  c("string", "unordered", "nominal")
}

#' @export
#' @rdname get_data_types
.get_data_types.ordered <- function(x) {
  c("ordered", "nominal")
}

#' @export
#' @rdname get_data_types
.get_data_types.factor <- function(x) {
  c("factor", "unordered", "nominal")
}

#' @export
#' @rdname get_data_types
.get_data_types.integer <- function(x) {
  c("integer", "numeric")
}

#' @export
#' @rdname get_data_types
.get_data_types.numeric <- function(x) {
  c("double", "numeric")
}

#' @export
#' @rdname get_data_types
.get_data_types.double <- function(x) {
  c("double", "numeric")
}

#' @export
#' @rdname get_data_types
.get_data_types.Surv <- function(x) {
  "surv"
}

#' @export
#' @rdname get_data_types
.get_data_types.logical <- function(x) {
  "logical"
}

#' @export
#' @rdname get_data_types
.get_data_types.Date <- function(x) {
  "date"
}

#' @export
#' @rdname get_data_types
.get_data_types.POSIXct <- function(x) {
  "datetime"
}

#' @export
#' @rdname get_data_types
.get_data_types.list <- function(x) {
  "list"
}

#' @export
#' @rdname get_data_types
.get_data_types.textrecipes_tokenlist <- function(x) {
  "tokenlist"
}

#' @export
#' @rdname get_data_types
.get_data_types.hardhat_case_weights <- function(x) {
  "case_weights"
}

all_get_data_types <- list(
  default = recipes:::.get_data_types.default(),
  character = recipes:::.get_data_types.character(),
  ordered = recipes:::.get_data_types.ordered(),
  factor = recipes:::.get_data_types.factor(),
  integer = recipes:::.get_data_types.integer(),
  numeric = recipes:::.get_data_types.numeric(),
  double = recipes:::.get_data_types.double(),
  Surv = recipes:::.get_data_types.Surv(),
  logical = recipes:::.get_data_types.logical(),
  Date = recipes:::.get_data_types.Date(),
  POSIXct = recipes:::.get_data_types.POSIXct(),
  list = recipes:::.get_data_types.list(),
  textrecipes_tokenlist = recipes:::.get_data_types.textrecipes_tokenlist(),
  hardhat_case_weights = recipes:::.get_data_types.hardhat_case_weights()
)

all_get_data_types <- lapply(
  all_get_data_types,
  glue::glue_collapse,
  sep = ", ",
  last = ", and "
)
