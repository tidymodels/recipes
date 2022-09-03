#' Time Feature Generator
#'
#' `step_time()` creates a *specification* of a recipe
#'  step that will convert date-time data into one or more factor or
#'  numeric variables.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables
#'  for this step. The selected variables should have class
#'  `POSIXct` or `POSIXlt`. See [selections()] for more details.
#' @param features A character string that includes at least one
#'  of the following values: `am` (is is AM), `hour`, `hour12`, `minute`,
#'  `second`, `decimal_day`.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [prep()] is used.
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `TRUE`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details Unlike some other steps, `step_time()` does *not*
#'  remove the original time variables by default. Set `keep_original_cols`
#'  to `FALSE` to remove them.
#'
#'  `decimal_day` return time of day as a decimal number between 0 and 24. for
#'  example `"07:15:00"` would be transformed to `7.25`  and `"03:59:59"` would
#'  be transformed to `3.999722`. The formula for these calculations are
#'  `hour(x) + (second(x) + minute(x) * 60) / 3600`.
#'
#'  See [step_date()] if you want to calculate features that are larger than
#'  hours.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or variables selected) and `value` (the feature
#'  names).
#'
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(
#'   times = ymd_hms("2022-05-06 23:51:07") +
#'   hours(1:5) + minutes(1:5) + seconds(1:5)
#' )
#' time_rec <- recipe(~ times, examples) %>%
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
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           features = c("hour", "minute", "second"),
           columns = NULL,
           keep_original_cols = TRUE,
           skip = FALSE,
           id = rand_id("time")) {
    feat <-
      c(
        "am",
        "hour",
        "hour12",
        "minute",
        "second",
        "decimal_day"
      )
    if (!is_tune(features)) {
      if (!all(features %in% feat)) {
        rlang::abort(paste0(
          "Possible values of `features` should include: ",
          paste0("'", feat, "'", collapse = ", ")
        ))
      }
    }
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
  function(terms, role, trained, features, columns, keep_original_cols, skip,
           id) {
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


#' @export
prep.step_time <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (!all(purrr::map_lgl(training[, col_names], inherits, "POSIXt"))) {
    rlang::abort(
      paste0(
        "All variables for `step_time` should be either `POSIXlt` or",
        "`POSIXct` classes."
      )
    )
  }

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
  check_new_data(names(object$columns), object, new_data)

  for (column in object$columns) {
    time_values <- get_time_features(
      dt = new_data[[column]],
      feats = object$features
    )

    names(time_values) <- glue::glue("{column}_{names(time_values)}")
    new_data <- bind_cols(new_data, time_values)
  }

  keep_original_cols <- get_keep_original_cols(object)
  if (!keep_original_cols) {
    new_data <- new_data[, !(colnames(new_data) %in% object$columns), drop = FALSE]
  }

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

  purrr::map_dfc(features[feats], ~.x(dt))
}


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
