#' Tidy the result of a recipe
#'
#' `tidy()` will return a data frame that contains information regarding a
#' recipe or operation within the recipe (when a `tidy()` method for the
#' operation exists).
#'
#' @name tidy.recipe
#'
#' @param x A `recipe` object, step, or check (trained or otherwise).
#' @param number An integer or `NA`. If missing, and `id` is not provided, the
#'   return value is a list of the operations in the recipe. If a number is
#'   given, a `tidy` method is executed for that operation in the recipe (if it
#'   exists). `number` must not be provided if `id` is.
#' @param id A character string or `NA`. If missing and `number` is not
#'   provided, the return value is a list of the operations in the recipe. If a
#'   character string is given, a `tidy` method is executed for that operation
#'   in the recipe (if it exists). `id` must not be provided if `number` is.
#' @param ... Not currently used.
#' @return A tibble with columns that vary depending on what `tidy` method is
#'   executed. When `number`, and `id` are `NA`, a tibble with columns `number`
#'   (the operation iteration), `operation` (either "step" or "check"), `type`
#'   (the method, e.g. "nzv", "center"), a logical column called `trained` for
#'   whether the operation has been estimated using `prep`, a logical for
#'   `skip`, and a character column `id`.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' Sacramento_rec <- recipe(~., data = Sacramento) |>
#'   step_other(all_nominal(), threshold = 0.05, other = "another") |>
#'   step_center(all_numeric()) |>
#'   step_dummy(all_nominal()) |>
#'   check_cols(ends_with("ude"), sqft, price)
#'
#' tidy(Sacramento_rec)
#'
#' tidy(Sacramento_rec, number = 2)
#' tidy(Sacramento_rec, number = 3)
#'
#' Sacramento_rec_trained <- prep(Sacramento_rec, training = Sacramento)
#'
#' tidy(Sacramento_rec_trained)
#' tidy(Sacramento_rec_trained, number = 3)
#' tidy(Sacramento_rec_trained, number = 4)
NULL

#' @rdname tidy.recipe
#' @export
tidy.recipe <- function(x, number = NA, id = NA, ...) {
  # add id = NA as default. If both ID & number are non-NA, error.
  # If number is NA and ID is not, select the step with the corresponding
  # ID. Only a single ID is allowed, as this follows the convention for number
  num_oper <- length(x$steps)
  pattern <- "(^step_)|(^check_)"

  check_string(id, allow_na = TRUE, allow_empty = FALSE)
  check_number_whole(number, allow_na = TRUE)

  if (!is.na(id)) {
    if (!is.na(number)) {
      cli::cli_abort(
        "You may specify {.arg number} or {.arg id}, but not both."
      )
    }
    step_ids <- vapply(x$steps, function(x) x$id, character(1))
    id <- rlang::arg_match(id, step_ids)
    number <- which(id == step_ids)
  }
  if (is.na(number)) {
    skipped <- vapply(x$steps, function(x) x$skip, logical(1))
    ids <- vapply(x$steps, function(x) x$id, character(1))

    oper_classes <- lapply(x$steps, class)
    oper_classes <- grep("_", unlist(oper_classes), value = TRUE)

    oper <- strsplit(oper_classes, split = "_")
    oper <- vapply(oper, function(x) x[1], character(1))

    oper_types <- gsub(pattern, "", oper_classes)
    is_trained <- vapply(
      x$steps,
      function(x) x$trained,
      logical(1)
    )
    res <- tibble(
      number = seq_along(x$steps),
      operation = oper,
      type = oper_types,
      trained = is_trained,
      skip = skipped,
      id = ids
    )
  } else {
    if (number > num_oper || length(number) > 1) {
      cli::cli_abort(
        "{.arg number} should be a single value between 1 and {num_oper},
         not {.obj_type_friendly {number}}."
      )
    }
    res <- tidy(x$steps[[number]], ...)
  }
  res
}

#' @rdname tidy.recipe
#' @export
tidy.step <- function(x, ...) {
  cli::cli_abort(
    "No {.cls tidy} method for a step with class{?es}: {.cls {class(x)}}."
  )
}

#' @rdname tidy.recipe
#' @export
tidy.check <- function(x, ...) {
  cli::cli_abort(
    "No {.cls tidy} method for a check with class{?es}: {.cls {class(x)}}."
  )
}
