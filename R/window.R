#' Moving window functions
#'
#' `step_window()` creates a *specification* of a recipe step that will create
#' new columns that are the results of functions that compute statistics across
#' moving windows.
#'
#' @inheritParams step_classdist
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? If `names` is left to be `NULL`, the rolling statistics
#'   replace the original columns and the roles are left unchanged. If `names`
#'   is set, those new columns will have a role of `NULL` unless this argument
#'   has a value.
#' @param size An odd integer `>= 3` for the window size.
#' @param na_rm A logical for whether missing values should be removed from the
#'   calculations within each window.
#' @param statistic A character string for the type of statistic that should be
#'   calculated for each moving window. Possible values are: `'max'`, `'mean'`,
#'   `'median'`, `'min'`, `'prod'`, `'sd'`, `'sum'`, `'var'`
#' @param names An optional character string that is the same length of the
#'   number of terms selected by `terms`. If you are not sure what columns will
#'   be selected, use the `summary` function (see the example below). These will
#'   be the names of the new columns created by the step.
#' @template step-return
#' @export
#' @details
#'
#' The calculations use a somewhat atypical method for handling the beginning
#' and end parts of the rolling statistics. The process starts with the center
#' justified window calculations and the beginning and ending parts of the
#' rolling values are determined using the first and last rolling values,
#' respectively. For example, if a column `x` with 12 values is smoothed with a
#' 5-point moving median, the first three smoothed values are estimated by
#' `median(x[1:5])` and the fourth uses `median(x[2:6])`.
#'
#' `keep_original_cols` also applies to this step if `names` is specified.
#'
#' This step requires the \pkg{RcppRoll} package. If not installed, the step
#' will stop with a note about installing the package.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `statistic`, `size` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{statistic}{character, the summary function name}
#'   \item{size}{integer, window size}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_window"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed(c("RcppML", "ggplot2"))
#' library(recipes)
#' library(dplyr)
#' library(rlang)
#' library(ggplot2, quietly = TRUE)
#'
#' set.seed(5522)
#' sim_dat <- data.frame(x1 = (20:100) / 10)
#' n <- nrow(sim_dat)
#' sim_dat$y1 <- sin(sim_dat$x1) + rnorm(n, sd = 0.1)
#' sim_dat$y2 <- cos(sim_dat$x1) + rnorm(n, sd = 0.1)
#' sim_dat$x2 <- runif(n)
#' sim_dat$x3 <- rnorm(n)
#'
#' rec <- recipe(y1 + y2 ~ x1 + x2 + x3, data = sim_dat) |>
#'   step_window(starts_with("y"),
#'     size = 7, statistic = "median",
#'     names = paste0("med_7pt_", 1:2),
#'     role = "outcome"
#'   ) |>
#'   step_window(starts_with("y"),
#'     names = paste0("mean_3pt_", 1:2),
#'     role = "outcome"
#'   )
#' rec <- prep(rec, training = sim_dat)
#'
#' smoothed_dat <- bake(rec, sim_dat)
#'
#' ggplot(data = sim_dat, aes(x = x1, y = y1)) +
#'   geom_point() +
#'   geom_line(data = smoothed_dat, aes(y = med_7pt_1)) +
#'   geom_line(data = smoothed_dat, aes(y = mean_3pt_1), col = "red") +
#'   theme_bw()
#'
#' tidy(rec, number = 1)
#' tidy(rec, number = 2)
#'
#' # If you want to replace the selected variables with the rolling statistic
#' # don't set `names`
#' sim_dat$original <- sim_dat$y1
#' rec <- recipe(y1 + y2 + original ~ x1 + x2 + x3, data = sim_dat) |>
#'   step_window(starts_with("y"))
#' rec <- prep(rec, training = sim_dat)
#' smoothed_dat <- bake(rec, sim_dat)
#' ggplot(smoothed_dat, aes(x = original, y = y1)) +
#'   geom_point() +
#'   theme_bw()
step_window <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    size = 3,
    na_rm = TRUE,
    statistic = "mean",
    columns = NULL,
    names = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("window")
  ) {
    if (!is_call(statistic)) {
      statistic <- rlang::arg_match(statistic, roll_funs)
    }

    add_step(
      recipe,
      step_window_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        size = size,
        na_rm = na_rm,
        statistic = statistic,
        columns = columns,
        names = names,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

roll_funs <- c("mean", "median", "sd", "var", "sum", "prod", "min", "max")

step_window_new <-
  function(
    terms,
    role,
    trained,
    size,
    na_rm,
    statistic,
    columns,
    names,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "window",
      terms = terms,
      role = role,
      trained = trained,
      size = size,
      na_rm = na_rm,
      statistic = statistic,
      columns = columns,
      names = names,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_window <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  ## ensure size is odd, integer, and not too small
  check_number_whole(x$size, arg = "size", min = 3)

  if (!is.integer(x$size)) {
    tmp <- x$size
    x$size <- as.integer(x$size)
    if (!isTRUE(all.equal(tmp, x$size))) {
      cli::cli_warn(
        "{.arg size} was not an integer ({tmp}) and was converted to {x$size}."
      )
    }
  }
  if (x$size %% 2 == 0) {
    cli::cli_abort("{.arg size} should be odd, not {x$size}.")
  }
  if (x$size < 3) {
    cli::cli_abort("{.arg size} should be at least 3, not {x$size}.")
  }

  if (!is.null(x$names)) {
    if (length(x$names) != length(col_names)) {
      cli::cli_abort(
        "There were {length(col_names)} term{?s} selected but
        {length(x$names)} value{?s} for the new features {?was/were} passed
        to {.arg names}."
      )
    }
  }

  step_window_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    size = x$size,
    na_rm = x$na_rm,
    statistic = x$statistic,
    columns = col_names,
    names = x$names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

roller <- function(x, stat = "mean", window = 3L, na_rm = TRUE) {
  recipes_pkg_check(required_pkgs.step_window())

  m <- length(x)

  gap <- floor(window / 2)
  if (m - window <= 2) {
    cli::cli_abort("The window is too large.")
  }

  ## stats for centered window
  opts <- list(
    x = x,
    n = window,
    by = 1L,
    fill = NA,
    partial = FALSE,
    normalize = TRUE,
    na.rm = na_rm
  )

  roll_cl <- call2(paste0("roll_", stat), !!!opts, .ns = "RcppRoll")
  x2 <- eval(roll_cl)

  ## Fill in the left-hand points. Add enough data so that the
  ## missing values at the start can be estimated and filled in
  x2[seq_len(gap)] <- x2[gap + 1]

  ## Right-hand points
  x2[(m - gap + 1):m] <- x2[m - gap]
  x2
}

#' @export
bake.step_window <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  new_values <- list()

  for (col_name in col_names) {
    new_values[[col_name]] <- roller(
      x = new_data[[col_name]],
      stat = object$statistic,
      na_rm = object$na_rm,
      window = object$size
    )
  }

  if (is.null(object$names)) {
    for (col_name in col_names) {
      new_data[[col_name]] <- new_values[[col_name]]
    }
  } else {
    names(new_values) <- object$names
    new_values <- tibble::new_tibble(new_values)
    new_values <- check_name(
      new_values,
      new_data,
      object,
      newname = object$names
    )
    new_data <- vec_cbind(new_data, new_values, .name_repair = "minimal")
    new_data <- remove_original_cols(new_data, object, col_names)
  }

  new_data
}

#' @export
print.step_window <-
  function(x, width = max(20, options()$width - 28), ...) {
    title <- glue("Moving {x$size}-point {x$statistic} on ")
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_window <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$statistic <- x$statistic
  out$size <- x$size
  out$id <- x$id
  out
}

#' @export
tunable.step_window <- function(x, ...) {
  tibble::tibble(
    name = c("statistic", "size"),
    call_info = list(
      list(pkg = "dials", fun = "summary_stat"),
      list(pkg = "dials", fun = "window_size")
    ),
    source = "recipe",
    component = "step_window",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_window <- function(x, ...) {
  c("RcppRoll")
}
