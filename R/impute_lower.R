#' Impute numeric data below the threshold of measurement
#'
#' `step_impute_lower()` creates a *specification* of a recipe step designed for
#' cases where the non-negative numeric data cannot be measured below a known
#' value. In these cases, one method for imputing the data is to substitute the
#' truncated value by a random uniform number between zero and the truncation
#' point.
#'
#' @inheritParams step_center
#' @param threshold A named numeric vector of lower bounds. This is `NULL` until
#'   computed by [prep()].
#' @template step-return
#' @family imputation steps
#' @export
#' @details
#'
#' `step_impute_lower()` estimates the variable minimums from the data used in
#' the `training` argument of [prep()]. [bake()] then simulates a value for any
#' data at the minimum with a random uniform value between zero and the minimum.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_lowerimpute()`
#' to `step_impute_lower()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the estimated value}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(biomass, package = "modeldata")
#'
#' ## Truncate some values to emulate what a lower limit of
#' ## the measurement system might look like
#'
#' biomass$carbon <- ifelse(biomass$carbon > 40, biomass$carbon, 40)
#' biomass$hydrogen <- ifelse(biomass$hydrogen > 5, biomass$carbon, 5)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' impute_rec <- rec |>
#'   step_impute_lower(carbon, hydrogen)
#'
#' tidy(impute_rec, number = 1)
#'
#' impute_rec <- prep(impute_rec, training = biomass_tr)
#'
#' tidy(impute_rec, number = 1)
#'
#' transformed_te <- bake(impute_rec, biomass_te)
#'
#' plot(transformed_te$carbon, biomass_te$carbon,
#'   ylab = "pre-imputation", xlab = "imputed"
#' )
step_impute_lower <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    threshold = NULL,
    skip = FALSE,
    id = rand_id("impute_lower")
  ) {
    add_step(
      recipe,
      step_impute_lower_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        threshold = threshold,
        skip = skip,
        id = id
      )
    )
  }

step_impute_lower_new <-
  function(terms, role, trained, threshold, skip, id) {
    step(
      subclass = "impute_lower",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_impute_lower <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  threshold <- vapply(training[, col_names], min, numeric(1), na.rm = TRUE)
  if (any(threshold < 0)) {
    offenders <- col_names[threshold < 0]

    cli::cli_abort(
      c(
        x = "The following columns negative values: {offenders}.",
        i = "Lower bound imputation is intended for data bounded at zero."
      )
    )
  }

  step_impute_lower_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = threshold,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_impute_lower <- function(object, new_data, ...) {
  col_names <- names(object$threshold)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    threshold <- object$threshold[[col_name]]
    affected <- which(new_data[[col_name]] <= threshold)

    if (length(affected) > 0) {
      new_data[[col_name]][affected] <- runif(
        length(affected),
        max = threshold
      )
    }
  }

  new_data
}

#' @export
print.step_impute_lower <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Lower bound imputation for "
    print_step(names(x$threshold), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_impute_lower <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$threshold),
      value = unname(x$threshold)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl)
  }
  res$id <- x$id
  res
}
