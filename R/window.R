#' Moving Window Functions
#'
#'   `step_window` creates a *specification* of a recipe
#'  step that will create new columns that are the results of
#'  functions that compute statistics across moving windows.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? If `names` is left to be
#'  `NULL`, the rolling statistics replace the original columns
#'  and the roles are left unchanged. If `names` is set, those
#'  new columns will have a role of `NULL` unless this argument
#'  has a value.
#' @param size An odd integer `>= 3` for the window size.
#' @param na.rm A logical for whether missing values should be
#'  removed from the calculations within each window.
#' @param statistic A character string for the type of statistic
#'  that should be calculated for each moving window. Possible
#'  values are: `'max'`, `'mean'`, `'median'`,
#'  `'min'`, `'prod'`, `'sd'`, `'sum'`,
#'  `'var'`
#' @param columns A character string that contains the names of
#'  columns that should be processed. These values are not
#'  determined until [prep.recipe()] is called.
#' @param names An optional character string that is the same
#'  length of the number of terms selected by `terms`. If you
#'  are not sure what columns will be selected, use the
#'  `summary` function (see the example below). These will be
#'  the names of the new columns created by the step.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `statistic` (the
#'  summary function name), and `size`.
#' @keywords datagen
#' @concept preprocessing moving_windows
#' @export
#' @details The calculations use a somewhat atypical method for
#'  handling the beginning and end parts of the rolling statistics.
#'  The process starts with the center justified window calculations
#'  and the beginning and ending parts of the rolling values are
#'  determined using the first and last rolling values,
#'  respectively. For example if a column `x` with 12 values is
#'  smoothed with a 5-point moving median, the first three smoothed
#'  values are estimated by `median(x[1:5])` and the fourth
#'  uses `median(x[2:6])`.
#' @examples
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
#' rec <- recipe(y1 + y2 ~ x1 + x2 + x3, data = sim_dat) %>%
#'   step_window(starts_with("y"), size = 7, statistic = "median",
#'               names = paste0("med_7pt_", 1:2),
#'               role = "outcome") %>%
#'   step_window(starts_with("y"),
#'               names = paste0("mean_3pt_", 1:2),
#'               role = "outcome")
#' rec <- prep(rec, training = sim_dat)
#'
#' # If you aren't sure how to set the names, see which variables are selected
#' # and the order that they are selected:
#' terms_select(info = summary(rec), terms = quos(starts_with("y")))
#'
#' smoothed_dat <- bake(rec, sim_dat, everything())
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
#' rec <- recipe(y1 + y2 + original ~ x1 + x2 + x3, data = sim_dat) %>%
#'   step_window(starts_with("y"))
#' rec <- prep(rec, training = sim_dat)
#' smoothed_dat <- bake(rec, sim_dat, everything())
#' ggplot(smoothed_dat, aes(x = original, y = y1)) +
#'   geom_point() +
#'   theme_bw()


step_window <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           size = 3,
           na.rm = TRUE,
           statistic = "mean",
           columns = NULL,
           names = NULL,
           skip = FALSE) {
    if(!(statistic %in% roll_funs) | length(statistic) != 1)
      stop("`statistic` should be one of: ",
           paste0("'", roll_funs, "'", collapse = ", "),
           call. = FALSE)

    ## ensure size is odd, integer, and not too small
    if (is.na(size) | is.null(size))
      stop("`size` needs a value.", call. = FALSE)

    if (!is.integer(size)) {
      tmp <- size
      size <- as.integer(size)
      if (!isTRUE(all.equal(tmp, size)))
        warning("`size` was not an integer (", tmp, ") and was ",
                "converted to ", size, ".", sep = "",
                call. = FALSE)
    }
    if (size %% 2 == 0)
      stop("`size` should be odd.", call. = FALSE)
    if (size < 3)
      stop("`size` should be at least 3.", call. = FALSE)

    add_step(
      recipe,
      step_window_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        size = size,
        na.rm = na.rm,
        statistic = statistic,
        columns = columns,
        names = names,
        skip = skip
      )
    )
  }

roll_funs <- c("mean", "median", "sd", "var", "sum", "prod", "min", "max")

step_window_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           size = NULL,
           na.rm = NULL,
           statistic = NULL,
           columns = NULL,
           names = names,
           skip = FALSE) {
    step(
      subclass = "window",
      terms = terms,
      role = role,
      trained = trained,
      size = size,
      na.rm = na.rm,
      statistic = statistic,
      columns = columns,
      names = names,
      skip = skip
    )
  }

#' @export
prep.step_window <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  if (any(info$type[info$variable %in% col_names] != "numeric"))
    stop("The selected variables should be numeric")

  if(!is.null(x$names)) {
    if(length(x$names) != length(col_names))
      stop("There were ", length(col_names), " term(s) selected but ",
           length(x$names), " values for the new features ",
           "were passed to `names`.", call. = FALSE)
  }

  step_window_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    size = x$size,
    na.rm = x$na.rm,
    statistic = x$statistic,
    columns = col_names,
    names = x$names,
    skip = x$skip
  )
}

#' @importFrom RcppRoll roll_max roll_maxl roll_maxr
#' @importFrom RcppRoll roll_mean roll_meanl roll_meanr
#' @importFrom RcppRoll roll_median roll_medianl roll_medianr
#' @importFrom RcppRoll roll_min roll_minl roll_minr
#' @importFrom RcppRoll roll_prod roll_prodl roll_prodr
#' @importFrom RcppRoll roll_sd roll_sdl roll_sdr
#' @importFrom RcppRoll roll_sum roll_suml roll_sumr
#' @importFrom RcppRoll roll_var roll_varl roll_varr
roller <- function(x, stat = "mean", window = 3L, na.rm = TRUE) {

  m <- length(x)

  gap <- floor(window / 2)
  if(m - window <= 2)
    stop("The window is too large.", call. = FALSE)

  ## stats for centered window
  roll_cl <- quote(
    roll_mean(
      x = x, n = window, weights = NULL, by = 1L,
      fill = NA, partial = FALSE,
      normalize = TRUE, na.rm = na.rm
    )
  )

  roll_cl[[1]] <- as.name(paste0("roll_", stat))
  x2 <- eval(roll_cl)

  ## Fill in the left-hand points. Add enough data so that the
  ## missing values at the start can be estimated and filled in
  x2[1:gap] <- x2[gap + 1]

  ## Right-hand points
  x2[(m - gap + 1):m] <- x2[m - gap]
  x2
}

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_window <- function(object, newdata, ...) {
  for (i in seq(along = object$columns)) {
    if (!is.null(object$names)) {
      newdata[, object$names[i]] <-
        roller(x = getElement(newdata, object$columns[i]),
               stat = object$statistic,
               na.rm = object$na.rm,
               window = object$size)
    } else {
      newdata[, object$columns[i]] <-
        roller(x = getElement(newdata, object$columns[i]),
               stat = object$statistic,
               na.rm = object$na.rm,
               window = object$size)
    }
  }
  newdata
}


print.step_window <-
  function(x, width = max(20, options()$width - 28), ...) {
    cat("Moving ", x$size, "-point ", x$statistic, " on ", sep = "")
    if (x$trained) {
      cat(format_ch_vec(x$columns, width = width))
    } else
      cat(format_selectors(x$terms, width = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_window
#' @param x A `step_window` object.
tidy.step_window <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$statistic <- x$statistic
  out$size <- x$size
  out
}
