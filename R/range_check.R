#' Check range consistency
#'
#' `check_range()` creates a *specification* of a recipe check that will check
#' if the range of a numeric variable changed in the new data.
#'
#' @inheritParams check_missing
#' @param slack_prop The allowed slack as a proportion of the range of the
#'   variable in the train set.
#' @param warn If `TRUE` the check will throw a warning instead of an error when
#'   failing.
#' @param lower A named numeric vector of minimum values in the train set. This
#'   is `NULL` until computed by [prep()].
#' @param upper A named numeric vector of maximum values in the train set. This
#'   is `NULL` until computed by [prep()].
#' @template check-return
#' @family checks
#' @export
#' @details
#'
#' The amount of slack that is allowed is determined by the `slack_prop`. This
#' is a numeric of length one or two. If of length one, the same proportion will
#' be used at both ends of the train set range. If of length two, its first
#' value is used to compute the allowed slack at the lower end, the second to
#' compute the allowed slack at the upper end.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this check, a tibble with columns `terms`
#' (the selectors or variables selected) and `value` (the means) is returned.
#'
#' @examples
#' slack_df <- data_frame(x = 0:100)
#' slack_new_data <- data_frame(x = -10:110)
#'
#' # this will fail the check both ends
#' \dontrun{
#' recipe(slack_df) |>
#'   check_range(x) |>
#'   prep() |>
#'   bake(slack_new_data)
#' }
#'
#' # this will fail the check only at the upper end
#' \dontrun{
#' recipe(slack_df) |>
#'   check_range(x, slack_prop = c(0.1, 0.05)) |>
#'   prep() |>
#'   bake(slack_new_data)
#' }
#'
#' # give a warning instead of an error
#' \dontrun{
#' recipe(slack_df) |>
#'   check_range(x, warn = TRUE) |>
#'   prep() |>
#'   bake(slack_new_data)
#' }
check_range <-
  function(
    recipe,
    ...,
    role = NA,
    skip = FALSE,
    trained = FALSE,
    slack_prop = 0.05,
    warn = FALSE,
    lower = NULL,
    upper = NULL,
    id = rand_id("range_check_")
  ) {
    add_check(
      recipe,
      check_range_new(
        terms = enquos(...),
        role = role,
        skip = skip,
        trained = trained,
        warn = warn,
        lower = lower,
        upper = upper,
        slack_prop = slack_prop,
        id = id
      )
    )
  }

## Initializes a new object
check_range_new <-
  function(terms, role, skip, trained, slack_prop, warn, lower, upper, id) {
    check(
      subclass = "range",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      warn = warn,
      lower = lower,
      upper = upper,
      slack_prop = slack_prop,
      id = id
    )
  }

#' @export
prep.check_range <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  ## TODO add informative error for nonnumerics

  lower_vals <- vapply(training[, col_names], min, c(min = 1), na.rm = TRUE)
  upper_vals <- vapply(training[, col_names], max, c(max = 1), na.rm = TRUE)
  check_range_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    warn = x$warn,
    lower = lower_vals,
    upper = upper_vals,
    slack_prop = x$slack_prop,
    id = x$id
  )
}

range_check_func <- function(
  x,
  lower,
  upper,
  slack_prop = 0.05,
  warn = FALSE,
  colname = "x"
) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} must be a numeric vector, not {.obj_type_friendly {x}}."
    )
  }

  if (!is.numeric(slack_prop)) {
    cli::cli_abort(
      "{.arg slack_prop} must be a numeric vector, \\
      not {.obj_type_friendly {slack_prop}}."
    )
  }

  min_x <- min(x)
  max_x <- max(x)

  if (length(slack_prop) == 1) {
    lower_allowed <- lower - ((upper - lower) * slack_prop)
    upper_allowed <- upper + ((upper - lower) * slack_prop)
  } else if (length(slack_prop) == 2) {
    lower_allowed <- lower - ((upper - lower) * slack_prop[1])
    upper_allowed <- upper + ((upper - lower) * slack_prop[2])
  } else {
    cli::cli_abort(
      "{.arg slack_prop} should be of length 1 or 2, not {length(slack_prop)}."
    )
  }

  msg <- NULL
  if (min_x < lower_allowed) {
    msg <- c(
      msg,
      i = "Smallest value of {.var {colname}} is {min_x}, \\
      crossing the lower bound {lower_allowed}."
    )
  }
  if (max_x > upper_allowed) {
    msg <- c(
      msg,
      i = "Largest value of {.var {colname}} is {max_x}, \\
      crossing the upper bound {upper_allowed}."
    )
  }

  if (!is.null(msg)) {
    if (warn) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(msg)
    }
  }
}

#' @export
bake.check_range <- function(object, new_data, ...) {
  col_names <- names(object$lower)
  check_new_data(col_names, object, new_data)

  if (nrow(new_data) == 0) {
    return(new_data)
  }

  for (col_name in col_names) {
    range_check_func(
      new_data[[col_name]],
      object$lower[col_name],
      object$upper[col_name],
      object$slack_prop,
      object$warn,
      col_name
    )
  }
  new_data
}

#' @export
print.check_range <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Checking range of "
    print_step(names(x$lower), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$lower))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
