#' Impute numeric data using a rolling window statistic
#'
#' `step_impute_roll()` creates a *specification* of a recipe step that will
#' substitute missing values of numeric variables by the measure of location
#' (e.g. median) within a moving window.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param ... One or more selector functions to choose variables to be imputed;
#'   these columns must be non-integer numerics (i.e., double precision). See
#'   [selections()] for more details.
#' @param window The size of the window around a point to be imputed. Should be
#'   an odd integer greater than one. See Details below for a discussion of
#'   points at the ends of the series.
#' @param statistic A function with a single argument for the data to compute
#'   the imputed value. Only complete values will be passed to the function and
#'   it should return a double precision value.
#' @template step-return
#' @family imputation steps
#' @family row operation steps
#' @export
#' @details
#'
#' On the tails, the window is shifted towards the ends. For example, for a
#' 5-point window, the windows for the first four points are `1:5`, `1:5`,
#' `1:5`, and then `2:6`.
#'
#' When missing data are in the window, they are not passed to the function. If
#' all of the data in the window are missing, a missing value is returned.
#'
#' The statistics are calculated on the training set values _before_ imputation.
#' This means that if previous data within the window are missing, their imputed
#' values are not included in the window data used for imputation. In other
#' words, each imputation does not know anything about previous imputations in
#' the series prior to the current point.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_rollimpute()`
#' to `step_impute_roll()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `window` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{window}{integer, window size}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_impute_roll"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @examples
#' library(lubridate)
#'
#' set.seed(145)
#' example_data <-
#'   data.frame(
#'     day = ymd("2012-06-07") + days(1:12),
#'     x1 = round(runif(12), 2),
#'     x2 = round(runif(12), 2),
#'     x3 = round(runif(12), 2)
#'   )
#' example_data$x1[c(1, 5, 6)] <- NA
#' example_data$x2[c(1:4, 10)] <- NA
#'
#' library(recipes)
#' seven_pt <- recipe(~., data = example_data) |>
#'   update_role(day, new_role = "time_index") |>
#'   step_impute_roll(all_numeric_predictors(), window = 7) |>
#'   prep(training = example_data)
#'
#' # The training set:
#' bake(seven_pt, new_data = NULL)
step_impute_roll <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    statistic = median,
    window = 5L,
    skip = FALSE,
    id = rand_id("impute_roll")
  ) {
    add_step(
      recipe,
      step_impute_roll_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        statistic = statistic,
        window = window,
        skip = skip,
        id = id
      )
    )
  }

step_impute_roll_new <-
  function(terms, role, trained, columns, statistic, window, skip, id) {
    step(
      subclass = "impute_roll",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      statistic = statistic,
      window = window,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_impute_roll <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = "double")
  check_function(x$statistic, arg = "statistic", )
  check_number_whole(x$window, arg = "window", min = 3)
  if (x$window %% 2 != 1) {
    cli::cli_abort("{.arg window} should be an odd integer >= 3.")
  }
  x$window <- as.integer(x$window)

  step_impute_roll_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    statistic = x$statistic,
    window = x$window,
    skip = x$skip,
    id = x$id
  )
}

get_window_ind <- function(i, n, k) {
  sides <- (k - 1) / 2
  if (i - sides >= 1 & i + sides <= n) {
    return((i - sides):(i + sides))
  }
  if (i - sides < 1) {
    return(1:k)
  }
  if (i + sides > n) {
    return((n - k + 1):n)
  }
}

get_rolling_ind <- function(inds, n, k) {
  map(inds, get_window_ind, n = n, k = k)
}
window_est <- function(inds, x, statfun) {
  x <- x[inds]
  x <- x[!is.na(x)]
  out <- if (length(x) == 0) {
    na_dbl
  } else {
    statfun(x)
  }
  if (!is.double(out)) {
    out <- as.double(out)
  }
  out
}
impute_rolling <- function(inds, x, statfun) {
  map_dbl(inds, window_est, x = x, statfun = statfun)
}

#' @export
bake.step_impute_roll <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  n <- nrow(new_data)
  missing_ind <- lapply(
    new_data[, col_names],
    function(x) which(is.na(x))
  )
  has_missing <- map_lgl(missing_ind, function(x) length(x) > 0)
  missing_ind <- missing_ind[has_missing]
  roll_ind <- lapply(missing_ind, get_rolling_ind, n = n, k = object$window)

  for (col_name in col_names) {
    estimates <- impute_rolling(
      roll_ind[[col_name]],
      new_data[[col_name]],
      object$statistic
    )
    new_data[missing_ind[[col_name]], col_name] <- estimates
  }

  new_data
}

#' @export
print.step_impute_roll <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Rolling imputation for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_impute_roll <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns), window = unname(x$window))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, window = unname(x$window))
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_impute_roll <- function(x, ...) {
  tibble::tibble(
    name = c("statistic", "window"),
    call_info = list(
      list(pkg = "dials", fun = "summary_stat"),
      list(pkg = "dials", fun = "window_size")
    ),
    source = "recipe",
    component = "step_impute_roll",
    component_id = x$id
  )
}
