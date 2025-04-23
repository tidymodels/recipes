#' Check for new values
#'
#' `check_new_values()` creates a *specification* of a recipe operation that
#' will check if variables contain new values.
#'
#' @inheritParams check_missing
#' @param ignore_NA A logical that indicates if we should consider missing
#'   values as value or not. Defaults to `TRUE`.
#' @param values A named list with the allowed values. This is `NULL` until
#'   computed by [prep()].
#' @template check-return
#' @family checks
#' @export
#' @details
#'
#' This check will break the [bake()] function if any of the checked columns
#' does contain values it did not contain when [prep()] was called on the
#' recipe. If the check passes, nothing is changed to the data.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this check, a tibble with columns `terms`
#' (the selectors or variables selected) is returned.
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(credit_data, package = "modeldata")
#'
#' # If the test passes, `new_data` is returned unaltered
#' recipe(credit_data) |>
#'   check_new_values(Home) |>
#'   prep() |>
#'   bake(new_data = credit_data)
#'
#' # If `new_data` contains values not in `x` at the [prep()] function,
#' # the [bake()] function will break.
#' \dontrun{
#' recipe(credit_data |> dplyr::filter(Home != "rent")) |>
#'   check_new_values(Home) |>
#'   prep() |>
#'   bake(new_data = credit_data)
#' }
#'
#' # By default missing values are ignored, so this passes.
#' recipe(credit_data |> dplyr::filter(!is.na(Home))) |>
#'   check_new_values(Home) |>
#'   prep() |>
#'   bake(credit_data)
#'
#' # Use `ignore_NA = FALSE` if you consider missing values  as a value,
#' # that should not occur when not observed in the train set.
#' \dontrun{
#' recipe(credit_data |> dplyr::filter(!is.na(Home))) |>
#'   check_new_values(Home, ignore_NA = FALSE) |>
#'   prep() |>
#'   bake(credit_data)
#' }
check_new_values <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    ignore_NA = TRUE,
    values = NULL,
    skip = FALSE,
    id = rand_id("new_values")
  ) {
    add_check(
      recipe,
      check_new_values_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        ignore_NA = ignore_NA,
        values = values,
        skip = skip,
        id = id
      )
    )
  }

check_new_values_new <-
  function(terms, role, trained, columns, skip, id, values, ignore_NA) {
    check(
      subclass = "new_values",
      prefix = "check_",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id,
      values = values,
      ignore_NA = ignore_NA
    )
  }

new_values_func <- function(
  x,
  allowed_values,
  colname = "x",
  ignore_NA = TRUE
) {
  new_vals <- setdiff(as.character(x), as.character(allowed_values))
  if (length(new_vals) == 0) {
    return()
  }
  if (all(is.na(new_vals)) && ignore_NA) {
    return()
  }
  if (ignore_NA) new_vals <- new_vals[!is.na(new_vals)]
  cli::cli_abort(
    "{.var {colname}} contains the new \\
    {cli::qty(length(new_vals))}value{?s}: {.val {new_vals}}."
  )
}

#' @export
prep.check_new_values <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_bool(x$ignore_NA, arg = "ignore_NA")

  values <- lapply(training[, col_names], unique)

  check_new_values_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id,
    values = values,
    ignore_NA = x$ignore_NA
  )
}

#' @export
bake.check_new_values <- function(object, new_data, ...) {
  col_names <- names(object$values)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_values_func(
      new_data[[col_name]],
      object$values[[col_name]],
      col_name,
      ignore_NA = object$ignore_NA
    )
  }

  new_data
}

#' @export
print.check_new_values <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Checking no new_values for "
    print_step(names(x$values), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_new_values <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
