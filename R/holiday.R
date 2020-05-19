#' Holiday Feature Generator
#'
#' `step_holiday` creates a a *specification* of a
#'  recipe step that will convert date data into one or more binary
#'  indicator variables for common holidays.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the new variables. The selected
#'  variables should have class `Date` or `POSIXct`. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param holidays A character string that includes at least one
#'  holiday supported by the `timeDate` package. See
#'  [timeDate::listHolidays()] for a complete list.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [prep.recipe()] is used.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the columns that will be affected and `holiday`.
#' @keywords datagen
#' @concept preprocessing
#' @concept model_specification
#' @concept variable_encodings
#' @concept dates
#' @export
#' @details Unlike other steps, `step_holiday` does
#'  *not* remove the original date variables.
#'  [step_rm()] can be used for this purpose.
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(someday = ymd("2000-12-20") + days(0:40))
#' holiday_rec <- recipe(~ someday, examples) %>%
#'    step_holiday(all_predictors())
#'
#' holiday_rec <- prep(holiday_rec, training = examples)
#' holiday_values <- bake(holiday_rec, new_data = examples)
#' holiday_values
#' @seealso [step_date()] [step_rm()]
#'   [recipe()] [prep.recipe()]
#'   [bake.recipe()] [timeDate::listHolidays()]
#' @import timeDate
step_holiday <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    holidays = c("LaborDay", "NewYearsDay", "ChristmasDay"),
    columns = NULL,
    skip = FALSE,
    id = rand_id("holiday")
  ) {

    if (!is_tune(holidays) & !is_varying(holidays)) {
      all_days <- listHolidays()
      if (!all(holidays %in% all_days))
        rlang::abort("Invalid `holidays` value. See timeDate::listHolidays")
    }

  add_step(
    recipe,
    step_holiday_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      holidays = holidays,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_holiday_new <-
  function(terms, role, trained, holidays, columns, skip, id) {
    step(
      subclass = "holiday",
      terms = terms,
      role = role,
      trained = trained,
      holidays = holidays,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_holiday <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  holiday_data <- info[info$variable %in% col_names, ]
  if (any(holiday_data$type != "date"))
    rlang::abort(
      paste0(
        "All variables for `step_holiday` should be either `Date` ",
        "or `POSIXct` classes."
         )
    )

  step_holiday_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    holidays = x$holidays,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}


is_holiday <- function(hol, dt) {
  hdate <- holiday(year = unique(year(dt)), Holiday = hol)
  hdate <- as.Date(hdate)
  out <- rep(0, length(dt))
  out[dt %in% hdate] <- 1
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
  for (i in seq_along(object$columns)) {
    tmp <- get_holiday_features(dt = new_data[[ object$columns[i] ]],
                                hdays = object$holidays)

    names(tmp) <- paste(object$columns[i], names(tmp), sep = "_")
    new_data <- bind_cols(new_data, tmp)
  }

  if (!is_tibble(new_data)) {
    new_data <- as_tibble(new_data)
  }
  new_data
}

print.step_holiday <-
  function(x, width = max(20, options()$width - 29), ...) {
    cat("Holiday features from ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_holiday
#' @param x A `step_holiday` object.
#' @export
tidy.step_holiday <- function(x, ...) {
  res <- simple_terms(x, ...)
  res <- expand.grid(terms = res$terms,
                     holiday = x$holidays,
                     stringsAsFactors = FALSE)
  res$id <- x$id
  as_tibble(res)
}
