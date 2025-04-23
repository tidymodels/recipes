#' Add sin and cos terms for harmonic analysis
#'
#' `step_harmonic()` creates a *specification* of a recipe step that will add
#' `sin()` and `cos()` terms for harmonic analysis.
#'
#' @inheritParams step_pca
#' @inheritParams step_date
#' @inheritParams step_center
#'
#' @param ... One or more selector functions to choose variables for this step.
#'   See [selections()] for more details. This will typically be a single
#'   variable.
#' @param frequency A numeric vector with at least one value. The value(s) must
#'   be greater than zero and finite.
#' @param cycle_size A numeric vector with at least one value that indicates the
#'   size of a single cycle. `cycle_size` should have the same units as the
#'   input variable(s).
#' @param starting_val either `NA`, numeric, Date or POSIXt value(s) that
#'   indicates the reference point for the sin and cos curves for each input
#'   variable. If the value is a `Date` or `POISXt` the value is converted to
#'   numeric using [as.numeric()]. This parameter may be specified to increase
#'   control over the signal phase.  If `starting_val` is not specified the
#'   default is 0.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' This step seeks to describe periodic components of observational data using a
#' combination of sin and cos waves. To do this, each wave of a specified
#' frequency is modeled using one sin and one cos term. The two terms for each
#' frequency can then be used to estimate the amplitude and phase shift of a
#' periodic signal in observational data. The equation relating cos waves of
#' known frequency but unknown phase and amplitude to a sum of sin and cos terms
#' is below:
#'
#' \deqn{A_j cos(\sigma_j t_i - \Phi_j) = C_j cos(\sigma_j t_i) + S_j
#' sin(\sigma_j t_i)}
#'
#' Solving the equation yields \eqn{C_j} and \eqn{S_j}. the amplitude can then
#' be obtained with:
#'
#' \deqn{A_j = \sqrt{C^2_j + S^2_j}}
#'
#' And the phase can be obtained with: \deqn{\Phi_j = \arctan{(S_j / C_j)}}
#'
#' where:
#'
#' * \eqn{\sigma_j = 2 \pi (frequency / cycle\_size))}
#' * \eqn{A_j} is the amplitude of the \eqn{j^{th}} frequency
#' * \eqn{\Phi_j} is the phase of the \eqn{j^{th}} frequency
#' * \eqn{C_j} is the coefficient of the cos term for the \eqn{j^{th}} frequency
#' * \eqn{S_j} is the coefficient of the sin term for the \eqn{j^{th}} frequency
#'
#' The periodic component is specified by `frequency` and `cycle_size`
#' parameters. The cycle size relates the specified frequency to the input
#' column(s) units. There are multiple ways to specify a wave of given
#' frequency, for example, a `POSIXct` input column given a `frequency` of 24
#' and a `cycle_size` equal to 86400 is equivalent to a `frequency` of 1.0 with
#' `cycle_size` equal to 3600.
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_harmonic"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `starting_val`, `cycle_size`, `frequency`, `key` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{starting_val}{numeric, the starting value}
#'   \item{cycle_size}{numeric, the cycle size}
#'   \item{frequency}{numeric, the frequency}
#'   \item{key}{character, key describing the calculation}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @references Doran, H. E., & Quilkey, J. J. (1972).
#'   Harmonic analysis of seasonal data: some important properties.
#'   American Journal of Agricultural Economics, 54, volume 4, part 1, 646-651.
#'
#' Foreman, M. G. G., & Henry, R. F. (1989).
#'   The harmonic analysis of tidal model time series.
#'   Advances in water resources, 12(3), 109-120.
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2, quietly = TRUE)
#' library(dplyr)
#'
#' data(sunspot.year)
#' sunspots <-
#'   tibble(
#'     year = 1700:1988,
#'     n_sunspot = sunspot.year,
#'     type = "measured"
#'   ) |>
#'   slice(1:75)
#'
#' # sunspots period is around 11 years, sample spacing is one year
#' dat <- recipe(n_sunspot ~ year, data = sunspots) |>
#'   step_harmonic(year, frequency = 1 / 11, cycle_size = 1) |>
#'   prep() |>
#'   bake(new_data = NULL)
#'
#' fit <- lm(n_sunspot ~ year_sin_1 + year_cos_1, data = dat)
#'
#' preds <- tibble(
#'   year = sunspots$year,
#'   n_sunspot = fit$fitted.values,
#'   type = "predicted"
#' )
#'
#' bind_rows(sunspots, preds) |>
#'   ggplot(aes(x = year, y = n_sunspot, color = type)) +
#'   geom_line()
#'
#'
#' # POSIXct example
#'
#' date_time <-
#'   as.POSIXct(
#'     paste0(rep(1959:1997, each = 12), "-", rep(1:12, length(1959:1997)), "-01"),
#'     tz = "UTC"
#'   )
#'
#' carbon_dioxide <- tibble(
#'   date_time = date_time,
#'   co2 = as.numeric(co2),
#'   type = "measured"
#' )
#'
#' # yearly co2 fluctuations
#' dat <-
#'   recipe(co2 ~ date_time,
#'     data = carbon_dioxide
#'   ) |>
#'   step_mutate(date_time_num = as.numeric(date_time)) |>
#'   step_ns(date_time_num, deg_free = 3) |>
#'   step_harmonic(date_time, frequency = 1, cycle_size = 86400 * 365.24) |>
#'   prep() |>
#'   bake(new_data = NULL)
#'
#' fit <- lm(co2 ~ date_time_num_ns_1 + date_time_num_ns_2 +
#'   date_time_num_ns_3 + date_time_sin_1 +
#'   date_time_cos_1, data = dat)
#'
#' preds <- tibble(
#'   date_time = date_time,
#'   co2 = fit$fitted.values,
#'   type = "predicted"
#' )
#'
#' bind_rows(carbon_dioxide, preds) |>
#'   ggplot(aes(x = date_time, y = co2, color = type)) +
#'   geom_line()
step_harmonic <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    frequency = NA_real_,
    cycle_size = NA_real_,
    starting_val = NA_real_,
    keep_original_cols = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("harmonic")
  ) {
    if (!all(is.numeric(cycle_size)) || all(is.na(cycle_size))) {
      msg <- c(
        x = "{.arg cycle_size} must have at least one non-NA numeric value."
      )

      if (!all(is.numeric(cycle_size))) {
        msg <- c(msg, i = "It was {.obj_type_friendly {cycle_size}}.")
      }

      cli::cli_abort(msg)
    }

    if (
      !all(is.na(starting_val)) &
        !all(is.numeric(starting_val)) &
        !all(inherits(starting_val, "Date")) &
        !all(inherits(starting_val, "POSIXt"))
    ) {
      cli::cli_abort(
        "starting_val must be NA, numeric, Date or POSIXt, \\
        not {.obj_type_friendly {starting_val}}.",
      )
    }

    add_step(
      recipe,
      step_harmonic_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        frequency = frequency,
        cycle_size = cycle_size,
        starting_val = starting_val,
        keep_original_cols = keep_original_cols,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_harmonic_new <-
  function(
    terms,
    role,
    trained,
    frequency,
    cycle_size,
    starting_val,
    columns,
    keep_original_cols,
    objects,
    skip,
    id
  ) {
    step(
      subclass = "harmonic",
      terms = terms,
      role = role,
      trained = trained,
      frequency = frequency,
      cycle_size = cycle_size,
      starting_val = starting_val,
      keep_original_cols = keep_original_cols,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_harmonic <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("date", "datetime", "numeric"))

  # check cycle_size
  if (length(x$cycle_size) == 1) {
    cycle_sizes <- rep(x$cycle_size, length(col_names))
  } else if (length(x$cycle_size) == length(col_names)) {
    cycle_sizes <- x$cycle_size
  } else {
    cli::cli_abort(
      "{.arg cycle_size} must be length 1 or the same length as the input \\
      columns."
    )
  }

  # check starting_val
  if (all(is.na(x$starting_val))) {
    starting_vals <- rep(0.0, length(col_names))
  } else if (length(x$starting_val) == 1) {
    starting_vals <- rep(as.numeric(x$starting_val), length(col_names))
  } else if (length(x$starting_val) == length(col_names)) {
    starting_vals <- x$starting_val
  } else {
    cli::cli_abort(
      "{.arg starting_val} must be length 1 or the same length as the input \\
      columns."
    )
  }

  frequencies <- sort(unique(na.omit(x$frequency)))

  names(frequencies) <- as.character(seq_along(frequencies))
  names(starting_vals) <- col_names
  names(cycle_sizes) <- col_names

  step_harmonic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    frequency = frequencies,
    cycle_size = cycle_sizes,
    starting_val = starting_vals,
    keep_original_cols = get_keep_original_cols(x),
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

sin_cos <- function(
  x,
  frequency,
  starting_val,
  cycle_size,
  call = caller_env()
) {
  nc <- length(frequency)

  if (length(x) == 0) {
    return(matrix(NA_real_, nrow = 0, ncol = nc * 2L))
  }

  if (all(is.na(x))) {
    cli::cli_abort("Variable must have at least one non-NA value.", call = call)
  }

  nr <- length(x)

  # adjust phase
  x <- x - as.numeric(starting_val)

  # cycles per unit
  cycle <- 2.0 * (pi * (x / cycle_size))

  m <- matrix(NA_real_, ncol = nc * 2L, nrow = nr)

  for (i in seq_along(frequency)) {
    m[, i] <- sin(cycle * frequency[i])
    m[, i + nc] <- cos(cycle * frequency[i])
  }

  return(m)
}

#' @export
bake.step_harmonic <- function(object, new_data, ...) {
  col_names <- names(object$starting_val)
  check_new_data(col_names, object, new_data)

  # calculate sin and cos columns
  for (col_name in col_names) {
    n_frequency <- length(object$frequency)
    res <- sin_cos(
      as.numeric(new_data[[col_name]]),
      object$frequency,
      object$starting_val[col_name],
      object$cycle_size[col_name]
    )
    colnames(res) <- paste0(
      col_name,
      rep(c("_sin_", "_cos_"), each = n_frequency),
      seq_len(n_frequency)
    )
    res <- as_tibble(res)

    res <- check_name(res, new_data, object, names(res))

    new_data <- vec_cbind(new_data, res, .name_repair = "minimal")
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_harmonic <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Harmonic numeric variables for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_harmonic <- function(x, ...) {
  if (is_trained(x)) {
    col_names <- names(x$starting_val)
    n_frequency <- length(x$frequency)
    n_terms <- length(col_names)

    res <-
      tibble(
        terms = rep(col_names, each = n_frequency * 2L),
        starting_val = rep(unname(x$starting_val), each = n_frequency * 2L),
        cycle_size = rep(unname(x$cycle_size), each = n_frequency * 2L),
        frequency = rep(rep(unname(x$frequency), times = 2L), times = n_terms),
      )
    res$key <- paste0(
      res$terms,
      rep(rep(c("_sin_", "_cos_"), each = n_frequency), times = n_terms),
      res$frequency
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      starting_val = na_dbl,
      cycle_size = na_dbl,
      frequency = na_dbl,
      key = na_chr
    )
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_harmonic <- function(x, ...) {
  tibble::tibble(
    name = "frequency",
    call_info = list(
      list(pkg = "dials", fun = "harmonic_frequency")
    ),
    source = "recipe",
    component = "step_harmonic",
    component_id = x$id
  )
}
