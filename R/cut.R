#' Cut a numeric variable into a factor
#'
#' `step_cut()` creates a *specification* of a recipe step that cuts a numeric
#'  variable into a factor based on provided boundary values
#'
#' @inheritParams step_center
#' @param breaks A numeric vector with at least one cut point.
#' @param include_outside_range Logical, indicating if values outside the
#'  range in the train set should be included in the lowest or highest bucket.
#'  Defaults to `FALSE`, values outside the original range will be set to `NA`.
#' @template step-return
#' @family discretization steps
#' @export
#' @details Unlike the `base::cut()` function there is no need to specify the
#'  min and the max values in the breaks. All values before the lowest break
#'  point will end up in the first bucket, all values after the last break
#'  points will end up in the last.
#'
#'  `step_cut()` will call `base::cut()` in the baking step with
#'  `include.lowest` set to `TRUE`.
#'
#' @template case-weights-not-supported
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 5:14)
#' rec <- recipe(df)
#'
#' # The min and max of the variable are used as boundaries
#' # if they exceed the breaks
#' rec %>%
#'   step_cut(x, breaks = 5) %>%
#'   prep() %>%
#'   bake(df)
#'
#' # You can use the same breaks on multiple variables
#' # then for each variable the boundaries are set separately
#' rec %>%
#'   step_cut(x, y, breaks = c(6, 9)) %>%
#'   prep() %>%
#'   bake(df)
#'
#' # You can keep the original variables using `step_mutate` or
#' # `step_mutate_at`, for transforming multiple variables at once
#' rec %>%
#'   step_mutate(x_orig = x) %>%
#'   step_cut(x, breaks = 5) %>%
#'   prep() %>%
#'   bake(df)
#'
#' # It is up to you if you want values outside the
#' # range learned at prep to be included
#' new_df <- data.frame(x = 1:11, y = 5:15)
#' rec %>%
#'   step_cut(x, breaks = 5, include_outside_range = TRUE) %>%
#'   prep() %>%
#'   bake(new_df)
#'
#' rec %>%
#'   step_cut(x, breaks = 5, include_outside_range = FALSE) %>%
#'   prep() %>%
#'   bake(new_df)
step_cut <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           breaks,
           include_outside_range = FALSE,
           skip = FALSE,
           id = rand_id("cut")) {
    add_step(
      recipe,
      step_cut_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        breaks = breaks,
        include_outside_range = include_outside_range,
        skip = skip,
        id = id
      )
    )
  }

step_cut_new <-
  function(terms, role, trained,
           breaks, include_outside_range, skip, id) {
    step(
      subclass = "cut",
      terms = terms,
      role = role,
      trained = trained,
      breaks = breaks,
      include_outside_range = include_outside_range,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_cut <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  all_breaks <- vector("list", length(col_names))
  names(all_breaks) <- col_names
  for (col_name in col_names) {
    all_breaks[[col_name]] <-
      create_full_breaks(training[[col_name]], breaks = x$breaks)
    full_breaks_check(all_breaks[[col_name]])
  }

  step_cut_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    breaks = all_breaks,
    include_outside_range = x$include_outside_range,
    skip = x$skip,
    id = x$id
  )
}

create_full_breaks <- function(var, breaks) {
  stopifnot(is.numeric(var), is.numeric(breaks))
  if (min(var) < min(breaks)) {
    breaks <- c(min(var), breaks)
  }
  if (max(var) > max(breaks)) {
    breaks <- c(max(var), breaks)
  }
  sort(breaks)
}

full_breaks_check <- function(breaks) {
  if (length(breaks) == 1) {
    rlang::abort("In step_cut: variable is invariant and equal to break point.")
  }
  if (length(breaks) == 2) {
    rlang::warn("In step_cut: this will create a factor with one value only.")
  }
}

#' @export
bake.step_cut <- function(object, new_data, ...) {
  check_new_data(names(object$breaks), object, new_data)

  for (col_name in names(object$breaks)) {
    res <- cut_var(
      new_data[[col_name]],
      object$breaks[[col_name]],
      object$include_outside_range
    )
    new_data[[col_name]] <- res
  }
  new_data
}

cut_var <- function(var, breaks, include_outside_range) {
  if (include_outside_range) {
    if (min(var) < min(breaks)) {
      breaks[1] <- min(var)
    }
    if (max(var) > max(breaks)) {
      breaks[length(breaks)] <- max(var)
    }
  }
  cutted_var <- cut(var, breaks, include.lowest = TRUE)
  if (include_outside_range) {
    cutted_var <- adjust_levels_min_max(cutted_var)
  }
  cutted_var
}

# this is necessary because bake.recipe does first learn
# original levels when prep.recipe is called and then reverts
# the levels when bake.recipe itself is called. Moreover,
# it is cleaner to show it in this way.
adjust_levels_min_max <- function(x) {
  stopifnot(is.factor(x))
  levs <- levels(x)
  if (length(levs) == 1) {
    return(factor(rep("[min,max]", length(x))))
  }
  first_level <- sub("(?<=\\[)(.*?)(?=,)", "min", levs[1], perl = TRUE)
  last_level <-
    sub("(?<=,)(.+?)(?=\\])", "max", levs[length(levs)], perl = TRUE)
  remaining_levs <- levs[-c(1, length(levs))]
  new_levs <- c(first_level, remaining_levs, last_level)
  names(new_levs) <- levs
  new_x <- new_levs[x]
  names(new_x) <- NULL
  names(new_levs) <- NULL
  factor(new_x, levels = new_levs)
}

print.step_cut <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Cut numeric for "
    print_step(names(x$breaks), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_cut <- function(x, ...) {
  if (is_trained(x)) {
    values <- vapply(
      unname(x$class_list),
      FUN = function(x) paste0(x, collapse = "-"),
      FUN.VALUE = character(1)
    )

    res <- tibble(terms = names(x$breaks), value = values)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_chr)
  }
  res$id <- x$id
  res
}
