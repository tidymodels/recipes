#' Scaling numeric data to a specific range
#'
#' `step_range()` creates a *specification* of a recipe step that will normalize
#' numeric data to be within a pre-defined range of values.
#'
#' @inheritParams step_center
#' @param min,max Single numeric values for the smallest (or largest) value in
#'   the transformed data.
#' @param clipping A single logical value for determining whether application of
#'   transformation onto new data should be forced to be inside `min` and `max`.
#'   Defaults to TRUE.
#' @param ranges A character vector of variables that will be normalized. Note
#'   that this is ignored until the values are determined by [prep()]. Setting
#'   this value will be ineffective.
#' @template step-return
#' @family normalization steps
#' @export
#' @details
#'
#' When a new data point is outside of the ranges seen in the training set, the
#' new values are truncated at `min` or `max`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `min`, `max` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{min}{numeric, lower range}
#'   \item{max}{numeric, upper range}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' ranged_trans <- rec |>
#'   step_range(carbon, hydrogen)
#'
#' ranged_obj <- prep(ranged_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ranged_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#'
#' tidy(ranged_trans, number = 1)
#' tidy(ranged_obj, number = 1)
step_range <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    min = 0,
    max = 1,
    clipping = TRUE,
    ranges = NULL,
    skip = FALSE,
    id = rand_id("range")
  ) {
    add_step(
      recipe,
      step_range_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        min = min,
        max = max,
        clipping = clipping,
        ranges = ranges,
        skip = skip,
        id = id
      )
    )
  }

step_range_new <-
  function(terms, role, trained, min, max, clipping, ranges, skip, id) {
    step(
      subclass = "range",
      terms = terms,
      role = role,
      trained = trained,
      min = min,
      max = max,
      clipping = clipping,
      ranges = ranges,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_range <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$min, arg = "min")
  check_number_decimal(x$max, arg = "max")
  check_bool(x$clipping, arg = "clipping")

  mins <-
    vapply(training[, col_names], min, c(min = 0), na.rm = TRUE)
  maxs <-
    vapply(training[, col_names], max, c(max = 0), na.rm = TRUE)

  inf_cols <- col_names[is.infinite(mins) | is.infinite(maxs)]
  if (length(inf_cols) > 0) {
    cli::cli_warn(
      "Column{?s} {.var {inf_cols}} returned NaN. \\
      Consider avoiding `Inf` values before normalising."
    )
  }
  zero_range_cols <- col_names[maxs - mins == 0]
  if (length(zero_range_cols) > 0) {
    cli::cli_warn(
      "Column{?s} {.var {zero_range_cols}} returned NaN. Consider using \\
       `step_zv()` to remove variables containing only a single value."
    )
  }

  step_range_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    min = x$min,
    max = x$max,
    clipping = x$clipping,
    ranges = rbind(mins, maxs),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_range <- function(object, new_data, ...) {
  col_names <- colnames(object$ranges)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    min <- object$ranges["mins", col_name]
    max <- object$ranges["maxs", col_name]

    new_data[[col_name]] <- (new_data[[col_name]] - min) *
      (object$max - object$min) /
      (max - min) +
      object$min

    if (is.null(object$clipping) || isTRUE(object$clipping)) {
      new_data[[col_name]] <- pmax(new_data[[col_name]], object$min)
      new_data[[col_name]] <- pmin(new_data[[col_name]], object$max)
    }
  }

  new_data
}

#' @export
print.step_range <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- glue("Range scaling to [{x$min},{x$max}] for ")
    print_step(colnames(x$ranges), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = colnames(x$ranges) %||% character(),
      min = unname(x$ranges["mins", ]),
      max = unname(x$ranges["maxs", ])
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      min = na_dbl,
      max = na_dbl
    )
  }
  res$id <- x$id
  res
}
