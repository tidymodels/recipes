#' Create a profiling version of a data set
#'
#' `step_profile()` creates a *specification* of a recipe step that will fix the
#' levels of all variables but one and will create a sequence of values for the
#' remaining variable. This step can be helpful when creating partial regression
#' plots for additive models.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param profile A bare name to specify which variable will be profiled (see
#'   [selections()]). Can also be a string or tidyselect for backwards
#'   compatibility. If a column is included in both lists to be fixed and to be
#'   profiled, an error is thrown.
#' @param pct A value between 0 and 1 that is the percentile to fix continuous
#'   variables. This is applied to all continuous variables captured by the
#'   selectors. For date variables, either the minimum, median, or maximum used
#'   based on their distance to `pct`.
#' @param index The level that qualitative variables will be fixed. If the
#'   variables are character (not factors), this will be the index of the sorted
#'   unique values. This is applied to all qualitative variables captured by the
#'   selectors.
#' @param grid A named list with elements `pctl` (a logical) and `len` (an
#'   integer). If `pctl = TRUE`, then `len` denotes how many percentiles to use
#'   to create the profiling grid. This creates a grid between 0 and 1 and the
#'   profile is determined by the percentiles of the data. For example, if `pctl
#'   = TRUE` and `len = 3`, the profile would contain the minimum, median, and
#'   maximum values. If `pctl = FALSE`, it defines how many grid points between
#'   the minimum and maximum values should be created. This parameter is ignored
#'   for qualitative variables (since all of their possible levels are
#'   profiled). In the case of date variables, `pctl = FALSE` will always be
#'   used since there is no quantile method for dates.
#' @details
#'
#' This step is atypical in that, when baked, the `new_data` argument is
#' ignored; the resulting data set is based on the fixed and profiled variable's
#' information.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `type` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{type}{character, `"fixed"` or `"profiled"`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @template step-return
#' @export
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2"))
#' data(Sacramento, package = "modeldata")
#'
#' # Setup a grid across beds but keep the other values fixed
#' recipe(~ city + price + beds, data = Sacramento) |>
#'   step_profile(-beds, profile = beds) |>
#'   prep(training = Sacramento) |>
#'   bake(new_data = NULL)
#'
#' ##########
#'
#' # An *additive* model; not for use when there are interactions or
#' # other functional relationships between predictors
#'
#' lin_mod <- lm(mpg ~ poly(disp, 2) + cyl + hp, data = mtcars)
#'
#' # Show the difference in the two grid creation methods
#'
#' disp_pctl <- recipe(~ disp + cyl + hp, data = mtcars) |>
#'   step_profile(-disp, profile = disp) |>
#'   prep(training = mtcars)
#'
#' disp_grid <- recipe(~ disp + cyl + hp, data = mtcars) |>
#'   step_profile(
#'     -disp,
#'     profile = disp,
#'     grid = list(pctl = FALSE, len = 100)
#'   ) |>
#'   prep(training = mtcars)
#'
#' grid_data <- bake(disp_grid, new_data = NULL)
#' grid_data <- grid_data |>
#'   mutate(
#'     pred = predict(lin_mod, grid_data),
#'     method = "grid"
#'   )
#'
#' pctl_data <- bake(disp_pctl, new_data = NULL)
#' pctl_data <- pctl_data |>
#'   mutate(
#'     pred = predict(lin_mod, pctl_data),
#'     method = "percentile"
#'   )
#'
#' plot_data <- bind_rows(grid_data, pctl_data)
#'
#' library(ggplot2)
#'
#' ggplot(plot_data, aes(x = disp, y = pred)) +
#'   geom_point(alpha = .5, cex = 1) +
#'   facet_wrap(~method)
step_profile <- function(
  recipe,
  ...,
  profile = NULL,
  pct = 0.5,
  index = 1,
  grid = list(pctl = TRUE, len = 100),
  columns = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("profile")
) {
  check_number_decimal(pct, min = 0, max = 1)

  if (length(grid) != 2) {
    cli::cli_abort(
      c(
        x = "`grid` should have 2 elements, not {length(grid)}.",
        i = "See {.help [?step_profile](recipes::step_profile)} for information."
      )
    )
  }
  if (!identical(sort(names(grid)), c("len", "pctl"))) {
    cli::cli_abort(
      c(
        x = "`grid` should have two named elements {.field len} and \\
          {.field pctl}, not {.val {sort(names(grid))}}.",
        i = "See {.help [?step_profile](recipes::step_profile)} for information."
      )
    )
  }

  check_number_whole(grid$len, min = 2)
  check_bool(grid$pctl)

  add_step(
    recipe,
    step_profile_new(
      terms = enquos(...),
      profile = enquos(profile),
      pct = pct,
      index = index,
      grid = grid,
      columns = columns,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_profile_new <-
  function(terms, profile, pct, index, grid, columns, role, trained, skip, id) {
    step(
      subclass = "profile",
      terms = terms,
      profile = profile,
      pct = pct,
      index = index,
      grid = grid,
      columns = columns,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_profile <- function(x, training, info = NULL, ...) {
  fixed_names <- recipes_eval_select(x$terms, training, info)
  profile_name <- recipes_argument_select(
    x$profile,
    training,
    info,
    arg_name = "profile"
  )

  if (length(profile_name) != 1) {
    msg <- c(x = "{.arg profile} should select only one column")

    if (length(profile_name) == 0) {
      msg <- c(msg, i = "No columns were selected.")
    } else {
      msg <- c(
        msg,
        i = "{length(profile_name)} columns were selected: \\
        {.var {profile_name}}."
      )
    }

    cli::cli_abort(msg)
  }

  if (any(profile_name == fixed_names)) {
    offenders <- fixed_names[profile_name == fixed_names]
    cli::cli_abort(
      "The profiled variable cannot be in the list of variables to be \\
      fixed. {.var {offenders}} was in both."
    )
  }
  fixed_vals <- lapply(
    training[, fixed_names],
    fixed,
    pct = x$pct,
    index = x$index
  )
  profile_vals <-
    list(prof(training[[profile_name]], grid = x$grid))
  names(profile_vals)[[1]] <- profile_name

  step_profile_new(
    terms = x$terms,
    role = x$role,
    profile = profile_vals,
    pct = x$pct,
    index = x$index,
    grid = x$grid,
    columns = fixed_vals,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_profile <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  profile_names <- names(object$profile)
  keepers <- c(col_names, profile_names)

  new_data <- list()

  for (col_name in col_names) {
    new_data[[col_name]] <- object$columns[[col_name]]
  }

  new_data[[names(object$profile)]] <- object$profile[[1]]

  new_data <- vctrs::vec_recycle_common(!!!new_data)
  new_data <- tibble::new_tibble(new_data)

  # Keep the predictors in the same order
  keepers <- names(new_data)[names(new_data) %in% keepers]
  new_data <- new_data[keepers]
  new_data
}

#' @export
print.step_profile <-
  function(x, width = max(20, options()$width - 22), ...) {
    title <- "Profiling data set for "
    print_step(names(x$profile), x$profile, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_profile <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
    fixed_names <- names(x$columns)
    prof_names <- names(x$profile)
  } else {
    fixed_names <- sel2char(x$terms)
    prof_names <- sel2char(x$profile)
  }

  if (length(fixed_names) == 0) {
    return(tibble(terms = character(), type = character(), id = character()))
  }
  fixed_res <- tibble(
    terms = fixed_names,
    type = rep("fixed", length = length(fixed_names))
  )
  prof_res <- tibble(
    terms = prof_names,
    type = rep("profiled", length = length(prof_names))
  )
  res <- bind_rows(fixed_res, prof_res)
  res$id <- x$id
  res
}

# some classes for the fixed values

#' Helper Functions for Profile Data Sets
#'
#' @param x A vector
#' @param pct,index,...,grid Options pass from [step_profile()]
#' @export
#' @keywords internal
fixed <- function(x, pct, index, ...) UseMethod("fixed")

#' @export
#' @rdname fixed
fixed.default <- function(x, pct, index, ...) {
  classes <- class(x)
  cli::cli_abort(
    "No method for determining a value to fix for objects of class{?es}: \\
    {.cls {classes}}."
  )
}
#' @export
#' @rdname fixed
fixed.numeric <- function(x, pct, index, ...) {
  res <- unname(quantile(x, probs = pct, na.rm = TRUE))
  if (is.integer(x)) {
    res <- unique(as.integer(res))
  }
  res
}
#' @export
#' @rdname fixed
fixed.factor <- function(x, pct, index, ...) {
  lev <- levels(x)[min(index, length(levels(x)))]
  factor(lev, levels = levels(x), ordered = is.ordered(x))
}
#' @export
#' @rdname fixed
fixed.character <- function(x, pct, index, ...) {
  x <- sort(unique(x))
  x[min(index, length(x))]
}

#' @export
#' @rdname fixed
fixed.Date <- function(x, pct, index, ...) {
  vals <- c(0, .5, 1)
  dst <- (vals - pct)^2
  mthd <- which.min(dst)
  if (mthd == 1) {
    out <- min(x, na.rm = TRUE)
  } else {
    if (mthd == 2) {
      out <- median(x, na.rm = TRUE)
    } else {
      out <- max(x, na.rm = TRUE)
    }
  }
  out
}
#' @export
#' @rdname fixed
fixed.POSIXct <- fixed.Date

#' @export
#' @rdname fixed
prof <- function(x, grid, ...) UseMethod("prof")

#' @export
#' @rdname fixed
prof.numeric <- function(x, grid, ...) {
  if (grid$pctl) {
    pct <- seq(0, 1, length = grid$len)
    out <- unname(quantile(x, probs = pct, na.rm = TRUE))
  } else {
    out <- seq(
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      length = grid$len
    )
  }
  if (is.integer(x)) {
    out <- as.integer(out)
  }
  unique(out)
}
#' @export
#' @rdname fixed
prof.factor <- function(x, grid, ...) {
  levels(x)
}
#' @export
#' @rdname fixed
prof.character <- function(x, grid, ...) {
  sort(unique(x))
}
#' @export
#' @rdname fixed
prof.Date <- function(x, grid, ...) {
  out <-
    seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid$len)
  unique(out)
}
#' @export
#' @rdname fixed
prof.POSIXct <- function(x, grid, ...) {
  x <- as.Date(x)
  prof.Date(x, grid, ...)
}
