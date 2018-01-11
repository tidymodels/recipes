#' Date Feature Generator
#'
#' `step_date` creates a a *specification* of a recipe
#'  step that will convert date data into one or more factor or
#'  numeric variables.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables that will be used to create the new variables. The
#'  selected variables should have class `Date` or
#'  `POSIXct`. See [selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param features A character string that includes at least one
#'  of the following values: `month`, `dow` (day of week),
#'  `doy` (day of year), `week`, `month`,
#'  `decimal` (decimal date, e.g. 2002.197), `quarter`,
#'  `semester`, `year`.
#' @param label A logical. Only available for features
#'  `month` or `dow`. `TRUE` will display the day of
#'  the week as an ordered factor of character strings, such as
#'  "Sunday." `FALSE` will display the day of the week as a
#'  number.
#' @param abbr A logical. Only available for features `month`
#'  or `dow`. `FALSE` will display the day of the week as
#'  an ordered factor of character strings, such as "Sunday".
#'  `TRUE` will display an abbreviated version of the label,
#'  such as "Sun". `abbr` is disregarded if `label =
#'  FALSE`.
#' @param ordinal A logical: should factors be ordered? Only
#'  available for features `month` or `dow`.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [prep.recipe()] is used.
#' @return For `step_date`, an updated version of recipe with
#'  the new step added to the sequence of existing steps (if any).
#'  For the `tidy` method, a tibble with columns `terms`
#'  (the selectors or variables selected), `value` (the feature
#'  names), and `ordinal` (a logical).
#' @keywords datagen
#' @concept preprocessing model_specification variable_encodings dates
#' @export
#' @details Unlike other steps, `step_date` does *not*
#'  remove the original date variables. [step_rm()] can be
#'  used for this purpose.
#' @examples
#' library(lubridate)
#'
#' examples <- data.frame(Dan = ymd("2002-03-04") + days(1:10),
#'                        Stefan = ymd("2006-01-13") + days(1:10))
#' date_rec <- recipe(~ Dan + Stefan, examples) %>%
#'    step_date(all_predictors())
#'
#' tidy(date_rec, number = 1)
#'
#' date_rec <- prep(date_rec, training = examples)
#'
#' date_values <- bake(date_rec, newdata = examples)
#' date_values
#'
#' tidy(date_rec, number = 1)
#'
#' @seealso [step_holiday()] [step_rm()]
#'   [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_date <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           features = c("dow", "month", "year"),
           abbr = TRUE,
           label = TRUE,
           ordinal = FALSE,
           columns = NULL,
           skip = FALSE
  ) {
  feat <-
    c("year",
      "doy",
      "week",
      "decimal",
      "semester",
      "quarter",
      "dow",
      "month")
  if (!all(features %in% feat))
    stop("Possible values of `features` should include: ",
         paste0("'", feat, "'", collapse = ", "))
  add_step(
    recipe,
    step_date_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      features = features,
      abbr = abbr,
      label = label,
      ordinal = ordinal,
      columns = columns,
      skip = skip
    )
  )
}

step_date_new <-
  function(
    terms = NULL,
    role = "predictor",
    trained = FALSE,
    features = features,
    abbr = abbr,
    label = label,
    ordinal = ordinal,
    columns = columns,
    skip = FALSE
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
    columns = columns,
    skip = skip
  )
}

#' @importFrom stats as.formula model.frame
#' @export
prep.step_date <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  date_data <- info[info$variable %in% col_names, ]
  if (any(date_data$type != "date"))
    stop("All variables for `step_date` should be either `Date` or",
         "`POSIXct` classes.", call. = FALSE)

  step_date_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    features = x$features,
    abbr = x$abbr,
    label = x$label,
    ordinal = x$ordinal,
    columns = col_names,
    skip = x$skip
  )
}


ord2fac <- function(x, what) {
  x <- getElement(x, what)
  factor(as.character(x), levels = levels(x), ordered = FALSE)
}


#' @importFrom lubridate year yday week decimal_date quarter semester wday month
get_date_features <-
  function(dt,
           feats,
           abbr = TRUE,
           label = TRUE,
           ord = FALSE) {
    ## pre-allocate values
    res <- matrix(NA, nrow = length(dt), ncol = length(feats))
    res <- as_tibble(res)
    colnames(res) <- feats

    if ("year" %in% feats)
      res[, grepl("year$", names(res))] <- year(dt)
    if ("doy" %in% feats)
      res[, grepl("doy$", names(res))] <- yday(dt)
    if ("week" %in% feats)
      res[, grepl("week$", names(res))] <- week(dt)
    if ("decimal" %in% feats)
      res[, grepl("decimal$", names(res))] <- decimal_date(dt)
    if ("quarter" %in% feats)
      res[, grepl("quarter$", names(res))] <- quarter(dt)
    if ("semester" %in% feats)
      res[, grepl("semester$", names(res))] <- semester(dt)
    if ("dow" %in% feats) {
      res[, grepl("dow$", names(res))] <-
        wday(dt, abbr = abbr, label = label)
      if (!ord & label == TRUE)
        res[, grepl("dow$", names(res))]  <-
          ord2fac(res, grep("dow$", names(res), value = TRUE))
    }
    if ("month" %in% feats) {
      res[, grepl("month$", names(res))] <-
        month(dt, abbr = abbr, label = label)
      if (!ord & label == TRUE)
        res[, grepl("month$", names(res))]  <-
          ord2fac(res, grep("month$", names(res), value = TRUE))
    }
    res
  }

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_date <- function(object, newdata, ...) {
  new_cols <-
    rep(length(object$features), each = length(object$columns))
  date_values <-
    matrix(NA, nrow = nrow(newdata), ncol = sum(new_cols))
  colnames(date_values) <- rep("", sum(new_cols))
  date_values <- as_tibble(date_values)

  strt <- 1
  for (i in seq_along(object$columns)) {
    cols <- (strt):(strt + new_cols[i] - 1)

    tmp <- get_date_features(
      dt = getElement(newdata, object$columns[i]),
      feats = object$features,
      abbr = object$abbr,
      label = object$label,
      ord = object$ordinal
    )

    date_values[, cols] <- tmp

    names(date_values)[cols] <-
      paste(object$columns[i],
            names(tmp),
            sep = "_")

    strt <- max(cols) + 1
  }
  newdata <- cbind(newdata, date_values)
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}


print.step_date <-
  function(x, width = max(20, options()$width - 29), ...) {
    cat("Date features from ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_date
#' @param x A `step_date` object.
tidy.step_date <- function(x, ...) {
  if (is_trained(x)) {
    res <- expand.grid(
      terms = x$columns,
      value = x$features,
      ordinal = x$ordinal
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- expand.grid(
      terms = term_names,
      value = x$features,
      ordinal = x$ordinal
    )
  }
  as_tibble(res)
}

