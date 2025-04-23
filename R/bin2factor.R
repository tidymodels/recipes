#' Create a factors from A dummy variable
#'
#' `step_bin2factor()` creates a *specification* of a recipe step that will
#' create a two-level factor from a single dummy variable.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param levels A length 2 character string that indicates the factor levels
#'   for the 1's (in the first position) and the zeros (second)
#' @param ref_first Logical. Should the first level, which replaces 1's, be the
#'   factor reference level?
#' @template step-return
#' @details
#'
#' This operation may be useful for situations where a binary piece of
#' information may need to be represented as categorical instead of numeric. For
#' example, naive Bayes models would do better to have factor predictors so that
#' the binomial distribution is modeled instead of a Gaussian probability
#' density of numeric binary data. Note that the numeric data is only verified
#' to be numeric (and does not count levels).
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @family dummy variable and encoding steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' data(covers, package = "modeldata")
#'
#' rec <- recipe(~description, covers) |>
#'   step_regex(description, pattern = "(rock|stony)", result = "rocks") |>
#'   step_regex(description, pattern = "(rock|stony)", result = "more_rocks") |>
#'   step_bin2factor(rocks)
#'
#' tidy(rec, number = 3)
#'
#' rec <- prep(rec, training = covers)
#' results <- bake(rec, new_data = covers)
#'
#' table(results$rocks, results$more_rocks)
#'
#' tidy(rec, number = 3)
step_bin2factor <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    levels = c("yes", "no"),
    ref_first = TRUE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("bin2factor")
  ) {
    if (length(levels) != 2 || !is.character(levels)) {
      msg <- c(x = "{.arg levels} should be a 2-element character string.")

      if (length(levels) != 2) {
        msg <- c(
          msg,
          i = "{length(levels)} element{?s} were supplied; two were expected."
        )
      }
      if (!is.character(levels)) {
        msg <- c(
          msg,
          i = "It was {.obj_type_friendly {levels}}."
        )
      }
      cli::cli_abort(msg)
    }
    check_bool(ref_first)

    add_step(
      recipe,
      step_bin2factor_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        levels = levels,
        ref_first = ref_first,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_bin2factor_new <-
  function(terms, role, trained, levels, ref_first, columns, skip, id) {
    step(
      subclass = "bin2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      ref_first = ref_first,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_bin2factor <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer", "logical"))

  step_bin2factor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    levels = x$levels,
    ref_first = x$ref_first,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_bin2factor <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  levs <- if (object$ref_first) object$levels else rev(object$levels)

  for (col_name in col_names) {
    tmp <- ifelse(
      new_data[[col_name]] == 1,
      object$levels[1],
      object$levels[2]
    )
    tmp <- factor(tmp, levels = levs)

    new_data[[col_name]] <- tmp
  }

  new_data
}

#' @export
print.step_bin2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Dummy variable to factor conversion for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_bin2factor <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
