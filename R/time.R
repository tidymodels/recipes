#' Time feature generator
#'
#' `step_time()` creates a *specification* of a recipe step that will convert
#' date-time data into one or more factor or numeric variables.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables for this step.
#'   The selected variables should have class `POSIXct` or `POSIXlt`. See
#'   [selections()] for more details.
#' @param features A character string that includes at least one of the
#'   following values: `am` (is is AM), `hour`, `hour12`, `minute`, `second`,
#'   `decimal_day`.
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `TRUE`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' Unlike some other steps, `step_time()` does *not* remove the original time
#' variables by default. Set `keep_original_cols` to `FALSE` to remove them.
#'
#' `decimal_day` return time of day as a decimal number between 0 and 24. for
#' example `"07:15:00"` would be transformed to `7.25`  and `"03:59:59"` would
#' be transformed to `3.999722`. The formula for these calculations are `hour(x)
#' + (second(x) + minute(x) * 60) / 3600`.
#'
#' See [step_date()] if you want to calculate features that are larger than
#' hours.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, the feature names}
#'   \item{id}{character, id of this step}
#' }
#'
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(
#'   times = ymd_hms("2022-05-06 23:51:07") +
#'   hours(1:5) + minutes(1:5) + seconds(1:5)
#' )
#' time_rec <- recipe(~ times, examples) |>
#'   step_time(all_predictors())
#'
#' tidy(time_rec, number = 1)
#'
#' time_rec <- prep(time_rec, training = examples)
#'
#' time_values <- bake(time_rec, new_data = examples)
#' time_values
#'
#' tidy(time_rec, number = 1)
step_time <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    features = c("hour", "minute", "second"),
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("time")
  ) {
    add_step(
      recipe,
      step_time_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        features = features,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_time_new <-
  function(
    terms,
    role,
    trained,
    features,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "time",
      terms = terms,
      role = role,
      trained = trained,
      features = features,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

feat_list <-
  c(
    "am",
    "hour",
    "hour12",
    "minute",
    "second",
    "decimal_day"
  )

#' @export
prep.step_time <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = "datetime")

  features <- x$features
  check_character(features, allow_na = FALSE)
  x$features <- rlang::arg_match(
    features,
    feat_list,
    multiple = TRUE,
    error_arg = "features"
  )

  step_time_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    features = x$features,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_time <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    time_values <- get_time_features(
      dt = new_data[[col_name]],
      feats = object$features
    )

    names(time_values) <- glue::glue("{col_name}_{names(time_values)}")
    time_values <- check_name(time_values, new_data, object, names(time_values))
    new_data <- vec_cbind(new_data, time_values, .name_repair = "minimal")
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

get_time_features <- function(dt, feats) {
  features <- list(
    am = am,
    hour = hour,
    hour12 = function(x) {
      out <- hour(x)
      out <- ifelse(out == 0L, 12L, out)
      out <- ifelse(out > 12L, out - 12L, out)
      out
    },
    minute = minute,
    second = second,
    decimal_day = function(x) hour(x) + (second(x) + minute(x) * 60) / 3600
  )

  res <- purrr::map(features[feats], \(f) f(dt))
  res <- vctrs::vec_cbind(!!!res)
  res
}

#' @export
print.step_time <-
  function(x, width = max(20, options()$width - 29), ...) {
    title <- "Time features from "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_time <- function(x, ...) {
  if (is_trained(x)) {
    res <- tidyr::crossing(
      terms = unname(x$columns),
      value = x$features
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tidyr::crossing(
      terms = term_names,
      value = x$features
    )
  }
  tibble::add_column(res, id = x$id)
}
