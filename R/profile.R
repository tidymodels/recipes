#' Create a Profiling Version of a Data Set
#'
#' `step_profile` creates a *specification* of a recipe step that
#'  will fix the levels of all variables but one and will create a
#'  sequence of values for the remaining variable. This step can be
#'  helpful when creating partial regression plots for additive
#'  models.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will fixed to a single value. See [selections()] for
#'  more details. For the `tidy` method, these are not currently
#'  used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param profile A call to [dplyr::vars()]) to specify which
#'  variable will be profiled (see [selections()]). If a column is
#'  included in both lists to be fixed and to be profiled, an error
#'  is thrown.
#' @param pct A value between 0 and 1 that is the percentile to
#'  fix continuous variables. This is applied to all continuous
#'  variables captured by the selectors. For date variables, either
#'  the minimum, median, or maximum used based on their distance to
#'  `pct`.
#' @param index The level that qualitative variables will be
#'  fixed. If the variables are character (not factors), this will
#'  be the index of the sorted unique values. This is applied to all
#'  qualitative variables captured by the selectors.
#' @param grid A named list with elements `pctl` (a logical) and
#'  `len` (an integer). If `pctl = TRUE`, then `len` denotes how
#'  many percentiles to use to create the profiling grid. This
#'  creates a grid between 0 and 1 and the profile is determined by
#'  the percentiles of the data. For example, if `pctl = TRUE` and
#'  `len = 3`, the profile would contain the minimum, median, and
#'  maximum values. If `pctl = FALSE`, it defines how many grid
#'  points between the minimum and maximum values should be created.
#'  This parameter is ignored for qualitative variables (since all
#'  of their possible levels are profiled). In the case of date
#'  variables, `pctl = FALSE` will always be used since there is no
#'  quantile method for dates.
#' @param columns A character string that contains the names of
#'  columns that should be fixed and their values. These values are
#'  not determined until [prep.recipe()] is called.
#' @details This step is atypical in that, when baked, the
#'  `new_data` argument is ignored; the resulting data set is
#'  based on the fixed and profiled variable's information.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (which
#'  is the columns that will be affected), and `type` (fixed or
#'  profiled).
#' @keywords datagen
#' @concept preprocessing
#' @export
#' @examples
#' data(okc)
#'
#' # Setup a grid across date but keep the other values fixed
#' recipe(~ diet + height + date, data = okc) %>%
#'   step_profile(-date, profile = vars(date)) %>%
#'   prep(training = okc, retain = TRUE) %>%
#'   juice
#'
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
#' disp_pctl <- recipe(~ disp + cyl + hp, data = mtcars) %>%
#'   step_profile(-disp, profile = vars(disp)) %>%
#'   prep(training = mtcars, retain = TRUE)
#'
#' disp_grid <- recipe(~ disp + cyl + hp, data = mtcars) %>%
#'   step_profile(
#'     -disp,
#'     profile = vars(disp),
#'     grid = list(pctl = FALSE, len = 100)
#'   ) %>%
#'   prep(training = mtcars, retain = TRUE)
#'
#' grid_data <- juice(disp_grid)
#' grid_data <- grid_data %>%
#'   mutate(pred = predict(lin_mod, grid_data),
#'          method = "grid")
#'
#' pctl_data <- juice(disp_pctl)
#' pctl_data <- pctl_data %>%
#'   mutate(pred = predict(lin_mod, pctl_data),
#'          method = "percentile")
#'
#' plot_data <- bind_rows(grid_data, pctl_data)
#'
#' library(ggplot2)
#'
#' ggplot(plot_data, aes(x = disp, y = pred)) +
#'   geom_point(alpha = .5, cex = 1) +
#'   facet_wrap(~ method)

step_profile <- function(recipe,
                         ...,
                         profile = NULL,
                         pct = 0.5,
                         index = 1,
                         grid = list(pctl = TRUE, len = 100),
                         columns = NULL,
                         role = NA,
                         trained = FALSE,
                         skip = FALSE,
                         id = rand_id("profile")) {

  if (pct < 0 | pct > 1)
    stop("`pct should be on [0, 1]`", call. = FALSE)
  if (length(grid) != 2)
    stop("`grid` should have two named elements. See ?step_profile",
         call. = FALSE)
  if (all(sort(names(grid)) == c("len", "ptcl")))
    stop("`grid` should have two named elements. See ?step_profile",
         call. = FALSE)
  if (grid$len < 2)
    stop("`grid$len should be at least 2.`", call. = FALSE)
  if (!is.logical(grid$pctl))
    stop("`grid$pctl should be logical.`", call. = FALSE)

  add_step(recipe,
           step_profile_new(
             terms = ellipse_check(...),
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
  fixed_names <- terms_select(x$terms, info = info)
  profile_name <- terms_select(x$profile, info = info)

  if(length(fixed_names) == 0)
    stop("At least one variable should be fixed", call. = FALSE)
  if(length(profile_name) != 1)
    stop("Only one variable should be profiled", call. = FALSE)
  if(any(profile_name == fixed_names))
    stop("The profiled variable cannot be in the list of ",
         "variables to be fixed.", call. = FALSE)
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
  n <- length(object$profile[[1]])
  new_data <- new_data[rep(1, n), ]
  keepers <- c(names(object$columns), names(object$profile))
  # Keep the predictors in the same order
  keepers <- names(new_data)[names(new_data) %in% keepers]
  new_data <- dplyr::select(new_data,! !keepers)

  for (i in names(object$columns)) {
    new_data[[i]] <- rep(object$columns[[i]], n)
  }
  new_data[[names(object$profile)]] <- object$profile[[1]]
  as_tibble(new_data)
}

print.step_profile <-
  function(x, width = max(20, options()$width - 22), ...) {
    cat("Profiling data set for  ")
    printer(names(x$profile), x$profile, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_profile
#' @param x A `step_profile` object.
#' @importFrom dplyr bind_rows
#' @export
tidy.step_profile <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
    fixed_names <- names(x$columns)
    prof_names <- names(x$profile)
  }
  else {
    fixed_names <- sel2char(x$terms)
    prof_names <- sel2char(x$profile)
  }
  fixed_res <- tibble(terms = fixed_names,
                      type = rep("fixed", length = length(fixed_names)))
  prof_res <- tibble(terms = prof_names,
                     type = rep("profiled", length = length(prof_names)))
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
fixed <- function (x, pct, index, ...) UseMethod("fixed")

#' @export
#' @rdname fixed
fixed.default <- function(x, pct, index, ...) {
  stop("No method for determining a value to fix for ",
       "objects of class(s) ",
       paste0("'", class(x), "'", collapse = ","),
       call. = FALSE)
}
#' @export
#' @rdname fixed
fixed.numeric <- function(x, pct, index, ...) {
  unname(quantile(x, probs = pct, na.rm = TRUE))
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
#' @importFrom stats median quantile
#' @export
#' @rdname fixed
fixed.Date <- function(x, pct, index, ...) {
  vals <- c(0, .5, 1)
  dst <- (vals - pct) ^ 2
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
prof <- function (x, grid, ...) UseMethod("prof")

#' @export
#' @rdname fixed
prof.numeric <- function(x, grid, ...) {
  if(grid$pctl) {
  pct <- seq(0, 1, length = grid$len)
  out <- unname(quantile(x, probs = pct, na.rm = TRUE))
  } else {
    out <- seq(
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      length = grid$len
    )
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





