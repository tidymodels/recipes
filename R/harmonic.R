#' Add sin and cos terms for harmonic analysis
#'
#' `step_harmonic` creates a *specification* of a recipe step that
#'   will add sin and cos terms for harmonic analysis.
#'
#' @inheritParams step_center
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created from the original variables will be
#'  used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param frequency A numeric vector with at least one value if using frequency
#'   to specify the cycle size.  The value must be greater than zero and finite.
#' @param period A numeric vector with at least one value if using period to
#'   specify the cycle size. The value must be greater than zero and finite.
#' @param cycle_unit Character string to indicate the units for a cycle.
#'   Possible values are:
#'   'year' (31556926 seconds), 'month_synodic' (2360592 seconds),
#'   'month_sidereal' (2551443 seconds), 'month_average' (2629744 seconds),
#'   'week', day', 'hour', 'minute', 'second', 'sample'.
#' @param starting_val either numeric, Date or POSIXt value that indicates the
#'   reference point for the sin and cos curves.  This parameter may be
#'   specified to increase control over the signal phase.  If starting_val is
#'   not specified the default is the start of the data series, or the start of
#'   the cycle (i.e. the beginning of the hour, day, week).
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `TRUE`.
#' @param objects Statistics are stored here once this step has
#'  been trained by [prep.recipe()].
#'
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @references Doran, H. E., & Quilkey, J. J. (1972).
#'   Harmonic analysis of seasonal data: some important properties.
#'   American Journal of Agricultural Economics, 54(4_Part_1), 646-651.
#'
#' @examples
#' library(ggplot2, quietly = TRUE)
#'
#' data(sunspot.year)
#' sunspots <- tibble(yr = 1700:1988,
#'                    n_sunspot = sunspot.year,
#'                    type = "measured")[1:75,]
#'
#' # sunspots period is around 11 years, sample spacing is one year
#' rec <- recipe(n_sunspot ~ yr,
#'               data = sunspots) %>%
#'               step_harmonic(yr, period = 11, cycle_unit = "sample") %>%
#'               prep() %>%
#'               bake(new_data = NULL)
#'
#'  fit <- lm(n_sunspot~yr_sin_p_11+yr_cos_p_11, rec)
#'
#'  preds <- tibble(yr = sunspots$yr,
#'                  n_sunspot = fit$fitted.values,
#'                  type = 'predicted')
#'
#'  ggplot(rbind(sunspots, preds), aes(x = yr, y = n_sunspot, color = type)) +
#'    geom_line()
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_harmonic <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           frequency = NA_real_,
           period = NA_real_,
           cycle_unit = 'day',
           cycle_size = NA_real_,
           starting_val = NA_real_,
           keep_original_cols = TRUE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("harmonic")) {

    if (!all(is.numeric(frequency)))
      rlang::abort("frequency value(s) must be numeric.")

    if (!all(is.numeric(period)))
      rlang::abort("period value(s) must be numeric.")

    if (all(is.na(period)) & all(is.na(frequency)))
      rlang::abort("period or frequency value(s) cannot both be NA")

    if (!all(is.na(period)) & any(period <= 0))
      rlang::abort("period value(s) must be greater than 0.")

    if (!all(is.na(frequency)) & any(frequency <= 0))
      rlang::abort("period value(s) must be greater than 0")

    if (any(is.infinite(frequency)))
      rlang::abort("frequency value(s) must be finite.")

    if (any(is.infinite(period)))
      rlang::abort("period value(s) must be finite.")

    if (!any(is.na(frequency)) & !any(is.na(period)))
      rlang::warn("Both frequency and period are specified.")

    if (!all(is.character(cycle_unit)) | length(cycle_unit) != 1)
      rlang::abort("cycle_unit column name must be a single character value.")

    if (!is.na(starting_val) &
        !is.numeric(starting_val) &
        !inherits(starting_val, 'Date') &
        !inherits(starting_val, 'POSIXt'))
      rlang::abort("starting_val must be numeric, Date or POSIXt")

    rlang::arg_match0(cycle_unit, c("year",
                                    "month_synodic",
                                    "month_sidereal",
                                    "month_average",
                                    "week",
                                    "day",
                                    "hour",
                                    "minute",
                                    "second",
                                    "sample"))

    # allow empty terms
    terms <- quos(...)
    if (is_empty(terms)) {
      terms = NULL
    }

    add_step(
      recipe,
      step_harmonic_new(
        terms = terms,
        trained = trained,
        role = role,
        frequency = frequency,
        period = period,
        cycle_unit = cycle_unit,
        cycle_size = cycle_size,
        starting_val = starting_val,
        keep_original_cols = keep_original_cols,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_harmonic_new <-
  function(terms, role, trained,
           frequency, period, cycle_unit, cycle_size,
           starting_val, keep_original_cols, objects, skip, id) {
    step(
      subclass = "harmonic",
      terms = terms,
      role = role,
      trained = trained,
      frequency = frequency,
      period = period,
      cycle_unit = cycle_unit,
      cycle_size = cycle_size,
      starting_val = starting_val,
      keep_original_cols = keep_original_cols,
      objects = objects,
      skip = skip,
      id = id
    )
  }


get_cycle_n <- function(x) {
  switch(x,
         year = 31556926,
         month_synodic = 2551443,
         month_sidereal = 2360592,
         week = 86400*7,
         day = 86400,
         hour = 3600,
         minute = 60,
         second = 1,
         sample = 1
  )
}


process_inputs <- function(time_var,
                           period = NA,
                           frequency = NA,
                           starting_val = NA,
                           cycle_unit = "week") {

  if (all(is.na(time_var)))
    rlang::abort("variable must have at least one non-NA value")


  # get cycle information
  cycle_size <- get_cycle_n(cycle_unit)
  if (inherits(time_var, 'Date')) {
    cycle_size <- cycle_size / 86400
  }

  # convert from period to frequency
  period_1    <- sort(unique(na.omit(period)))
  frequency_1 <- sort(unique(na.omit(frequency)))
  frequency_2 <- 1.0 / period_1
  frequencies <- c(frequency_1, frequency_2)

  # set column names

  if (length(frequency_1) > 0 & length(period_1) > 0) {
    nms <- c(paste0('_f_', frequency_1), paste0('_p_', period_1))
  } else if (length(frequency_1) > 0) {
    nms <- paste0('_f_', frequency_1)
  } else if (length(period_1) > 0) {
    nms <- paste0('_p_', period_1)
  }

  col_names <- c(paste0(rep("sin", each = length(frequencies)),
                        1:length(frequencies)),
                 paste0(rep("cos", each = length(frequencies)),
                        1:length(frequencies)))


  # start at an even cycle if no starting_val is provided
  if (is.na(starting_val)) {
    mn <- min(time_var, na.rm = TRUE)
    starting_val <- (as.numeric(mn) %/% cycle_size) * cycle_size
  }


  list(
    starting_val = starting_val,
    frequency = frequencies,
    col_names = col_names,
    cycle_size = cycle_size
  )

}




#' @export
prep.step_harmonic <- function(x, training, info = NULL, ...) {

  # check period and frequency info

  col_names <- eval_select_recipes(x$terms, training, info)
  harmonic_data <- info[info$variable %in% col_names, ]

  if (any(harmonic_data$type != "date" & harmonic_data$type != "numeric"))
    rlang::abort(
      paste0(
        "All variables for `step_harmonic` should be either `Date` ",
        "`POSIXct` or `numeric` classes."
      )
    )

  # record the starting value for consistent phases
  x$object <- lapply(training[, col_names], process_inputs,
                           period = x$period,
                           frequency = x$frequency,
                           starting_val = x$starting_val,
                           cycle_unit = x$cycle_unit)

  names(x$object) <- col_names


  step_harmonic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    frequency = x$frequency,
    period = x$period,
    cycle_unit = x$cycle_unit,
    starting_val = x$object$starting_val,
    keep_original_cols = get_keep_original_cols(x),
    object = x$object,
    skip = x$skip,
    id = x$id
  )
}


sin_cos <- function(x,
                    frequency,
                    starting_val,
                    cycle_size = 86400) {

  if (all(is.na(x)))
    rlang::abort("variable must have at least one non-NA value")

  nc <- length(frequency)
  nr <- length(x)

  x <- x - as.numeric(starting_val)

  # cycles per unit
  cycle <- 2 * (pi * (x / cycle_size));

  m <- matrix(NA_real_,
         ncol = nc * 2,
         nrow = nr)

  for(i in seq_along(frequency)) {
    m[, i] <- sin(cycle * frequency[i])
    m[, i + nc] <- cos(cycle * frequency[i])
  }


  return(m)
}



#' @export
bake.step_harmonic <- function(object, new_data, ...) {

  for (col_name in names(object$objects)) {
    ob <- object$objects[[col_name]]
    res <- sin_cos(as.numeric(new_data[[col_name]]),
                   ob[["frequency"]],
                   ob[["starting_val"]],
                   ob[["cycle_size"]])
    colnames(res) <- paste0(col_name, '_', ob[["col_names"]])
    res <- as_tibble(res)
    new_data <- bind_cols(new_data, res)
  }


  keep_original_cols <- get_keep_original_cols(object)
  if (!keep_original_cols) {
    new_data <- new_data[, !(colnames(new_data) %in% object$columns), drop = FALSE]
  }


  as_tibble(new_data)
}

#' @export
print.step_harmonic <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Harmonic numeric variables for ", sep = "")
    printer(tr_obj = names(x$object), untr_obj = x$terms,
            trained = x$trained, width = width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @param x A `step_harmonic` object.
#' @export
tidy.step_harmonic <- function(x, ...) {
  if (is_trained(x)) {
    res <-
      tibble(terms = names(x$objects),
             value = sapply(x$objects, function(y) y$starting_val),
             key = 0, # column names
             frequency = 0) # frequencies
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl,
                  key = na_chr,
                  frequency = na_dbl)
  }
  res$id <- x$id
  res
}


# may want a tunable step that can adjust the frequency/period
#' #' @rdname tunable.step
#' #' @export
#' tunable.step_harmonic <- function(x, ...) {
#'   tibble::tibble(
#'     name = c("frequency", "period"),
#'     call_info = list(
#'       list(pkg = "dials", fun = "harmonic_period"),
#'       list(pkg = "dials", fun = "harmonic_period")
#'     ),
#'     source = "recipe",
#'     component = "step_harmonic",
#'     component_id = x$id
#'   )
#' }
