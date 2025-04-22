#' Logarithmic transformation
#'
#' `step_log()` creates a *specification* of a recipe step that will log
#' transform data.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param base A numeric value for the base.
#' @param offset An optional value to add to the data prior to logging (to avoid
#'   `log(0)`).
#' @param signed A logical indicating whether to take the signed log. This is
#'   `sign(x) * log(abs(x))` when `abs(x) => 1` or `0 if abs(x) < 1`. If `TRUE`
#'   the `offset` argument will be ignored.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `base` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{base}{numeric, value for the base}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examples
#' set.seed(313)
#' examples <- matrix(exp(rnorm(40)), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' log_trans <- rec |>
#'   step_log(all_numeric_predictors())
#'
#' log_obj <- prep(log_trans, training = examples)
#'
#' transformed_te <- bake(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(log_trans, number = 1)
#' tidy(log_obj, number = 1)
#'
#' # using the signed argument with negative values
#'
#' examples2 <- matrix(rnorm(40, sd = 5), ncol = 2)
#' examples2 <- as.data.frame(examples2)
#'
#' recipe(~ V1 + V2, data = examples2) |>
#'   step_log(all_numeric_predictors()) |>
#'   prep(training = examples2) |>
#'   bake(examples2)
#'
#' recipe(~ V1 + V2, data = examples2) |>
#'   step_log(all_numeric_predictors(), signed = TRUE) |>
#'   prep(training = examples2) |>
#'   bake(examples2)
step_log <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    base = exp(1),
    offset = 0,
    columns = NULL,
    skip = FALSE,
    signed = FALSE,
    id = rand_id("log")
  ) {
    add_step(
      recipe,
      step_log_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        base = base,
        offset = offset,
        columns = columns,
        skip = skip,
        signed = signed,
        id = id
      )
    )
  }

step_log_new <-
  function(terms, role, trained, base, offset, columns, skip, signed, id) {
    step(
      subclass = "log",
      terms = terms,
      role = role,
      trained = trained,
      base = base,
      offset = offset,
      columns = columns,
      skip = skip,
      signed = signed,
      id = id
    )
  }

#' @export
prep.step_log <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$offset, arg = "offset")
  check_bool(x$signed, arg = "signed")
  check_number_decimal(x$base, arg = "base", min = 0)

  step_log_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    base = x$base,
    offset = x$offset,
    columns = col_names,
    skip = x$skip,
    signed = x$signed,
    id = x$id
  )
}

#' @export
bake.step_log <- function(object, new_data, ...) {
  # For backward compatibility #1284
  col_names <- names(object$columns) %||% object$columns
  check_new_data(col_names, object, new_data)

  # for backward compat
  if (all(names(object) != "offset")) {
    object$offset <- 0
  }

  if (object$signed && object$offset != 0) {
    cli::cli_warn("When {.arg signed} is TRUE, {.arg offset} will be ignored.")
  }

  for (col_name in col_names) {
    tmp <- new_data[[col_name]]

    if (object$signed) {
      tmp <- ifelse(
        abs(tmp) < 1,
        0,
        sign(tmp) * log(abs(tmp), base = object$base)
      )
    } else {
      tmp <- log(tmp + object$offset, base = object$base)
    }

    new_data[[col_name]] <- tmp
  }

  new_data
}

#' @export
print.step_log <-
  function(x, width = max(20, options()$width - 31), ...) {
    msg <- ifelse(x$signed, "Signed log", "Log")
    title <- glue("{msg} transformation on ")
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_log <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$base <- x$base
  out$id <- x$id
  out
}
