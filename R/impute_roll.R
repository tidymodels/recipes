#' Impute numeric data using a rolling window statistic
#'
#' `step_impute_roll` creates a *specification* of a
#'  recipe step that will substitute missing values of numeric
#'  variables by the measure of location (e.g. median) within a moving window.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables to be imputed;
#'  these columns must be non-integer numerics (i.e., double precision).
#'  See [selections()] for more details.
#' @param columns A named numeric vector of columns. This is
#'  `NULL` until computed by [prep.recipe()].
#' @param window The size of the window around a point to be imputed. Should be
#'  an odd integer greater than one. See Details below for a discussion of
#'  points at the ends of the series.
#' @param statistic A function with a single argument for the data to compute
#'  the imputed value. Only complete values will be passed to the function and
#'  it should return a double precision value.
#' @template step-return
#' @family imputation steps
#' @family row operation steps
#' @export
#' @details On the tails, the window is shifted towards the ends.
#'  For example, for a 5-point window, the windows for the first
#'  four points are `1:5`, `1:5`, `1:5`, and then `2:6`.
#'
#'   When missing data are in the window, they are not passed to the
#'  function. If all of the data in the window are missing, a
#'  missing value is returned.
#'
#'   The statistics are calculated on the training set values
#'  _before_ imputation. This means that if previous data within the
#'  window are missing, their imputed values are not included in the
#'  window data used for imputation. In other words, each imputation
#'  does not know anything about previous imputations in the series
#'  prior to the current point.
#'
#'  When you [`tidy()`] this step, a tibble with columns `terms` (the
#'  selectors or variables selected) and `window` (the window size) is returned.
#'
#'  As of `recipes` 0.1.16, this function name changed from `step_rollimpute()`
#'    to `step_impute_roll()`.
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
#' seven_pt <- recipe(~ . , data = example_data) %>%
#'   update_role(day, new_role = "time_index") %>%
#'   step_impute_roll(all_numeric_predictors(), window = 7) %>%
#'   prep(training = example_data)
#'
#' # The training set:
#' bake(seven_pt, new_data = NULL)

step_impute_roll <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           statistic = median,
           window = 5,
           skip = FALSE,
           id = rand_id("impute_roll")) {

    if (!is_tune(window) & !is_varying(window)) {
      if (window < 3 | window %% 2 != 1) {
        rlang::abort("`window` should be an odd integer >= 3")
      }
      window <- as.integer(floor(window))
    }

    add_step(
      recipe,
      step_impute_roll_new(
        terms = ellipse_check(...),
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

#' @rdname step_impute_roll
#' @export
step_rollimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           statistic = median,
           window = 5,
           skip = FALSE,
           id = rand_id("impute_roll")) {
    lifecycle::deprecate_warn(
      when = "0.1.16",
      what = "recipes::step_rollimpute()",
      with = "recipes::step_impute_roll()"
    )
    step_impute_roll(
      recipe,
      ...,
      role = role,
      trained = trained,
      columns = columns,
      statistic = statistic,
      window = window,
      skip = skip,
      id = id
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
  check_type(training[, col_names])
  dbl_check <- vapply(training[, col_names], is.double, logical(1))
  if (any(!dbl_check))
    rlang::abort("All columns must be double precision for rolling imputation")

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

#' @export
#' @keywords internal
prep.step_rollimpute <- prep.step_impute_roll

get_window_ind <- function(i, n, k) {
  sides <- (k - 1) / 2
  if (i - sides >= 1 & i + sides <= n)
    return((i - sides):(i + sides))
  if (i - sides < 1)
    return(1:k)
  if (i + sides > n)
    return((n - k + 1):n)
}

get_rolling_ind <- function(inds, n, k)
  map(inds, get_window_ind, n = n, k = k)
window_est <- function(inds, x, statfun) {
  x <- x[inds]
  x <- x[!is.na(x)]
  out <- if(length(x) == 0)
    na_dbl
  else
    statfun(x)
  if(!is.double(out))
    out <- as.double(out)
  out
}
impute_rolling <- function(inds, x, statfun) {
  map_dbl(inds, window_est, x = x, statfun = statfun)
}

#' @export
bake.step_impute_roll <- function(object, new_data, ...) {
  n <- nrow(new_data)
  missing_ind <- lapply(new_data[, object$columns],
                        function(x) which(is.na(x)))
  has_missing <- map_lgl(missing_ind, function(x) length(x) > 0)
  missing_ind <- missing_ind[has_missing]
  roll_ind <- lapply(missing_ind, get_rolling_ind, n = n, k = object$window)

  for(i in seq(along.with = roll_ind)) {
    imp_var <- names(roll_ind)[i]
    estimates <-
      impute_rolling(roll_ind[[i]], new_data[[imp_var]], object$statistic)
    new_data[missing_ind[[i]], imp_var] <- estimates
  }
  as_tibble(new_data)
}

#' @export
#' @keywords internal
bake.step_rollimpute <- bake.step_impute_roll

#' @export
print.step_impute_roll <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Rolling Imputation for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
#' @keywords internal
print.step_rollimpute <- print.step_impute_roll

#' @rdname tidy.recipe
#' @export
tidy.step_impute_roll <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns), window = x$window)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, window = x$window)
  }
  res$id <- x$id
  res
}

#' @export
#' @keywords internal
tidy.step_rollimpute <- tidy.step_impute_roll

#' @rdname tunable.recipe
#' @export
tunable.step_impute_roll <- function(x, ...) {
  tibble::tibble(
    name = c("statistic", "window"),
    call_info = list(
      list(pkg = "dials", fun = "location_stat"),
      list(pkg = "dials", fun = "window")
    ),
    source = "recipe",
    component = "step_impute_roll",
    component_id = x$id
  )
}

#' @export
#' @keywords internal
tunable.step_rollimpute <- tunable.step_impute_roll
