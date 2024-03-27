#' Holiday feature generator
#'
#' `step_holiday()` creates a *specification* of a recipe step that will convert
#' date data into one or more binary indicator variables for common holidays.
#'
#' @inheritParams step_date
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param holidays A character string that includes at least one
#'  holiday supported by the `timeDate` package. See
#'  [timeDate::listHolidays()] for a complete list.
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [timeDate::listHolidays()]
#' @export
#' @details Unlike some other steps, `step_holiday` does *not*
#'  remove the original date variables by default. Set `keep_original_cols`
#'  to `FALSE` to remove them.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `holiday` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{holiday}{character, name of holidays}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(someday = ymd("2000-12-20") + days(0:40))
#' holiday_rec <- recipe(~someday, examples) %>%
#'   step_holiday(all_predictors())
#'
#' holiday_rec <- prep(holiday_rec, training = examples)
#' holiday_values <- bake(holiday_rec, new_data = examples)
#' holiday_values
#' @import timeDate
step_holiday <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           holidays = c("LaborDay", "NewYearsDay", "ChristmasDay"),
           columns = NULL,
           keep_original_cols = TRUE,
           skip = FALSE,
           id = rand_id("holiday")) {
    if (!is_tune(holidays)) {
      all_days <- listHolidays()
      if (!all(holidays %in% all_days)) {
        cli::cli_abort(c(
          "Invalid {.arg holidays} value. \\
          See {.fn timeDate::listHolidays} for possible values."
        ))
      }
    }

    add_step(
      recipe,
      step_holiday_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        holidays = holidays,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_holiday_new <-
  function(terms, role, trained, holidays, columns, keep_original_cols, skip, id) {
    step(
      subclass = "holiday",
      terms = terms,
      role = role,
      trained = trained,
      holidays = holidays,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_holiday <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("date", "datetime"))

  step_holiday_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    holidays = x$holidays,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

is_holiday <- function(hol, dt) {
  years <- unique(year(dt))
  na_year <- which(is.na(years))
  if (length(na_year) > 0) {
    years <- years[-na_year]
  }
  hdate <- holiday(year = years, Holiday = hol)
  hdate <- as.Date(hdate)
  out <- rep(0, length(dt))
  out[dt %in% hdate] <- 1
  out[is.na(dt)] <- NA
  out
}

get_holiday_features <- function(dt, hdays) {
  if (!is.Date(dt)) {
    dt <- as.Date(dt)
  }
  hdays <- as.list(hdays)
  hfeat <- lapply(hdays, is_holiday, dt = dt)
  hfeat <- do.call("cbind", hfeat)
  colnames(hfeat) <- unlist(hdays)
  as_tibble(hfeat)
}

#' @export
bake.step_holiday <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    tmp <- get_holiday_features(
      dt = new_data[[col_name]],
      hdays = object$holidays
    )

    names(tmp) <- paste(col_name, names(tmp), sep = "_")
    tmp <- purrr::map(tmp, vec_cast, integer())
    tmp <- vctrs::vec_cbind(!!!tmp)

    tmp <- check_name(tmp, new_data, object, names(tmp))
    new_data <- vec_cbind(new_data, tmp)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_holiday <-
  function(x, width = max(20, options()$width - 29), ...) {
    title <- "Holiday features from "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_holiday <- function(x, ...) {
  res <- simple_terms(x, ...)
  res <- tidyr::expand_grid(terms = res$terms, holiday = x$holidays)
  res$id <- x$id
  res
}
