#' Date feature generator
#'
#' `step_date()` creates a *specification* of a recipe step that will convert
#' date data into one or more factor or numeric variables.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables for this step.
#'   The selected variables should have class `Date` or `POSIXct`. See
#'   [selections()] for more details.
#' @param features A character string that includes at least one of the
#'   following values: `month`, `dow` (day of week), `mday` (day of month),
#'   `doy` (day of year), `week`, `month`, `decimal` (decimal date, e.g.
#'   2002.197), `quarter`, `semester`, `year`.
#' @param label A logical. Only available for features `month` or `dow`. `TRUE`
#'   will display the day of the week as an ordered factor of character strings,
#'   such as "Sunday." `FALSE` will display the day of the week as a number.
#' @param abbr A logical. Only available for features `month` or `dow`. `FALSE`
#'   will display the day of the week as an ordered factor of character strings,
#'   such as "Sunday". `TRUE` will display an abbreviated version of the label,
#'   such as "Sun". `abbr` is disregarded if `label = FALSE`.
#' @param ordinal A logical: should factors be ordered? Only available for
#'   features `month` or `dow`.
#' @param locale Locale to be used for `month` and `dow`, see [locales]. On
#'   Linux systems you can use `system("locale -a")` to list all the installed
#'   locales. Can be a locales string, or a [clock::clock_labels()] object.
#'   Defaults to `clock::clock_locale()$labels`.
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `TRUE`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' Unlike some other steps, `step_date()` does *not* remove the original date
#' variables by default. Set `keep_original_cols` to `FALSE` to remove them.
#'
#' See [step_time()] if you want to calculate features that are smaller than
#' days.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, `ordinal` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, the feature names}
#'   \item{ordinal}{logical, are factors ordered}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(
#'   Dan = ymd("2002-03-04") + days(1:10),
#'   Stefan = ymd("2006-01-13") + days(1:10)
#' )
#' date_rec <- recipe(~ Dan + Stefan, examples) |>
#'   step_date(all_predictors())
#'
#' tidy(date_rec, number = 1)
#'
#' date_rec <- prep(date_rec, training = examples)
#'
#' date_values <- bake(date_rec, new_data = examples)
#' date_values
#'
#' tidy(date_rec, number = 1)
step_date <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    features = c("dow", "month", "year"),
    abbr = TRUE,
    label = TRUE,
    ordinal = FALSE,
    locale = clock::clock_locale()$labels,
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("date")
  ) {
    feat <-
      c(
        "year",
        "doy",
        "mday",
        "week",
        "decimal",
        "semester",
        "quarter",
        "dow",
        "month"
      )
    if (!is_tune(features)) {
      if (!all(features %in% feat)) {
        offenders <- features[!features %in% feat]

        cli::cli_abort(
          c(
            x = "Possible values of {.arg features} are {.or {.val {feat}}}.",
            i = "Invalid values were: {.val {offenders}}."
          )
        )
      }
    }
    add_step(
      recipe,
      step_date_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        features = features,
        abbr = abbr,
        label = label,
        ordinal = ordinal,
        locale = locale,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_date_new <-
  function(
    terms,
    role,
    trained,
    features,
    abbr,
    label,
    ordinal,
    locale,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "date",
      terms = terms,
      role = role,
      trained = trained,
      features = features,
      abbr = abbr,
      label = label,
      ordinal = ordinal,
      locale = locale,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_date <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("date", "datetime"))
  check_bool(x$abbr, arg = "abbr")
  check_bool(x$label, arg = "label")
  check_bool(x$ordinal, arg = "ordinal")

  step_date_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    features = x$features,
    abbr = x$abbr,
    label = x$label,
    ordinal = x$ordinal,
    locale = x$locale,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

ord2fac <- function(x, what) {
  x <- x[[what]]
  factor(as.character(x), levels = levels(x), ordered = FALSE)
}

get_date_features <-
  function(dt, feats, locale, abbr = TRUE, label = TRUE, ord = FALSE) {
    ## pre-allocate values
    res <- matrix(NA_integer_, nrow = length(dt), ncol = length(feats))
    colnames(res) <- feats
    res <- as_tibble(res)

    if ("year" %in% feats) {
      res[, grepl("year$", names(res))] <- vec_cast(year(dt), integer())
    }
    if ("doy" %in% feats) {
      res[, grepl("doy$", names(res))] <- vec_cast(yday(dt), integer())
    }
    if ("mday" %in% feats) {
      res[, grepl("mday$", names(res))] <- vec_cast(mday(dt), integer())
    }
    if ("week" %in% feats) {
      res[, grepl("week$", names(res))] <- vec_cast(week(dt), integer())
    }
    if ("decimal" %in% feats) {
      res[, grepl("decimal$", names(res))] <- decimal_date(dt)
    }
    if ("quarter" %in% feats) {
      res[, grepl("quarter$", names(res))] <- vec_cast(quarter(dt), integer())
    }
    if ("semester" %in% feats) {
      res[, grepl("semester$", names(res))] <- vec_cast(semester(dt), integer())
    }
    if ("dow" %in% feats) {
      if (inherits(locale, "clock_labels")) {
        dow <- clock::date_weekday_factor(
          x = dt,
          abbreviate = abbr,
          labels = locale
        )
        if (!label) {
          dow <- as.integer(dow)
        }
        res[, grepl("dow$", names(res))] <- dow
      } else {
        res[, grepl("dow$", names(res))] <-
          wday(dt, abbr = abbr, label = label, locale = locale)
      }
      if (!ord & label == TRUE) {
        res[, grepl("dow$", names(res))] <-
          ord2fac(res, grep("dow$", names(res), value = TRUE))
      }
    }
    if ("month" %in% feats) {
      if (inherits(locale, "clock_labels")) {
        month <- clock::date_month_factor(
          dt,
          abbreviate = abbr,
          labels = locale
        )
        if (!label) {
          month <- as.integer(month)
        }
        res[, grepl("month$", names(res))] <- month
      } else {
        res[, grepl("month$", names(res))] <-
          month(dt, abbr = abbr, label = label, locale = locale)
      }
      if (!ord & label == TRUE) {
        res[, grepl("month$", names(res))] <-
          ord2fac(res, grep("month$", names(res), value = TRUE))
      }
    }
    res
  }

#' @export
bake.step_date <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  new_cols <- rep(
    length(object$features),
    each = length(col_names)
  )
  names(new_cols) <- col_names

  date_values <- matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))

  # Dummy column names to avoid tibble warning
  colnames(date_values) <- as.character(seq_len(sum(new_cols)))

  date_values <- as_tibble(date_values)

  new_names <- vector("character", length = ncol(date_values))

  strt <- 1
  for (col_name in col_names) {
    cols <- (strt):(strt + new_cols[col_name] - 1)

    tmp <- get_date_features(
      dt = new_data[[col_name]],
      feats = object$features,
      locale = object$locale %||% Sys.getlocale("LC_TIME"),
      abbr = object$abbr,
      label = object$label,
      ord = object$ordinal
    )

    date_values[, cols] <- tmp

    new_names[cols] <- paste(col_name, names(tmp), sep = "_")

    strt <- max(cols) + 1
  }

  names(date_values) <- new_names

  date_values <- check_name(date_values, new_data, object, names(date_values))
  new_data <- vec_cbind(new_data, date_values, .name_repair = "minimal")

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_date <-
  function(x, width = max(20, options()$width - 29), ...) {
    title <- "Date features from "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_date <- function(x, ...) {
  if (is_trained(x)) {
    res <- tidyr::crossing(
      terms = unname(x$columns),
      value = x$features,
      ordinal = x$ordinal
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tidyr::crossing(
      terms = term_names,
      value = x$features,
      ordinal = x$ordinal
    )
  }
  tibble::add_column(res, id = x$id)
}
