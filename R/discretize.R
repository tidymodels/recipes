#' Discretize Numeric Variables
#'
#' `discretize()` converts a numeric vector into a factor with bins having
#' approximately the same number of data points (based on a training set).
#'
#' @export
#' @param x A numeric vector
discretize <- function(x, ...) {
  UseMethod("discretize")
}

#' @export
#' @rdname discretize
discretize.default <- function(x, ...) {
  cli::cli_abort(
    c(
      x = "Only numeric {.arg x} is accepted.",
      i = "The {.arg x} was passed {.obj_type_friendly {x}}."
    )
  )
}

#' @rdname discretize
#' @param cuts An integer defining how many cuts to make of the data.
#' @param labels A character vector defining the factor levels that will be in
#'   the new factor (from smallest to largest). This should have length `cuts+1`
#'   and should not include a level for missing (see `keep_na` below).
#' @param prefix A single parameter value to be used as a prefix for the factor
#'   levels (e.g. `bin1`, `bin2`, ...). If the string is not a valid R name, it
#'   is coerced to one. If `prefix = NULL` then the factor levels will be
#'   labelled according to the output of [cut()].
#' @param keep_na A logical for whether a factor level should be created to
#'   identify missing values in `x`. If `keep_na` is set to `TRUE` then `na.rm =
#'   TRUE` is used when calling [stats::quantile()].
#' @param infs A logical indicating whether the smallest and largest cut point
#'   should be infinite.
#' @param min_unique An integer defining a sample size line of dignity for the
#'   binning. If `(the number of unique values)/(cuts+1)` is less than
#'   `min_unique`, no discretization takes place.
#' @param ... Options to pass to [stats::quantile()] that should not include `x`
#'   or `probs`.
#' @return `discretize` returns an object of class `discretize` and
#'   [predict.discretize()] returns a factor vector.
#' @export
#' @details
#'
#' `discretize()` estimates the cut points from `x` using percentiles. For
#' example, if `cuts = 3`, the function estimates the quartiles of `x` and uses
#' these as the cut points. If `cuts = 2`, the bins are defined as being above
#' or below the median of `x`.
#'
#' The [predict()] method can then be used to turn numeric vectors into factor
#' vectors.
#'
#' If `keep_na = TRUE`, a suffix of `"_missing"` is used as a factor level (see
#' the examples below).
#'
#' If `infs = FALSE` and a new value is greater than the largest value of `x`, a
#' missing value will result.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' median(biomass_tr$carbon)
#' discretize(biomass_tr$carbon, cuts = 2)
#' discretize(biomass_tr$carbon, cuts = 2, infs = FALSE)
#' discretize(biomass_tr$carbon, cuts = 2, infs = FALSE, keep_na = FALSE)
#' discretize(biomass_tr$carbon, cuts = 2, prefix = "maybe a bad idea to bin")
#'
#' carbon_binned <- discretize(biomass_tr$carbon)
#' table(predict(carbon_binned, biomass_tr$carbon))
#'
#' carbon_no_infs <- discretize(biomass_tr$carbon, infs = FALSE)
#' predict(carbon_no_infs, c(50, 100))
discretize.numeric <-
  function(
    x,
    cuts = 4,
    labels = NULL,
    prefix = "bin",
    keep_na = TRUE,
    infs = TRUE,
    min_unique = 10,
    ...
  ) {
    unique_vals <- length(unique(x))
    missing_lab <- "_missing"
    check_number_whole(cuts, min = 2)

    dots <- list(...)
    if (keep_na) {
      dots$na.rm <- TRUE
    }

    if (unique_vals / (cuts + 1) >= min_unique) {
      cl <- rlang::call2(
        "quantile",
        .ns = "stats",
        x = x,
        probs = seq(0, 1, length = cuts + 1)
      )
      cl <- rlang::call_modify(cl, !!!dots)
      breaks <- rlang::eval_tidy(cl)
      num_breaks <- length(breaks)
      breaks <- unique(breaks)
      if (num_breaks > length(breaks)) {
        cli::cli_warn(
          "Not enough data for {cuts} breaks. Only {length(breaks)} breaks
           were used."
        )
      }
      if (infs) {
        breaks[1] <- -Inf
        breaks[length(breaks)] <- Inf
      }
      breaks <- unique(breaks)

      if (is.null(labels)) {
        prefix <- prefix[1]
        if (make.names(prefix) != prefix && !is.null(prefix)) {
          cli::cli_warn(
            "The prefix {.val {prefix}} is not a valid R name. It has been
            changed to {.val {make.names(prefix)}}."
          )
          prefix <- make.names(prefix)
        }
        labels <- names0(length(breaks) - 1, "")
      }
      out <- list(
        breaks = breaks,
        bins = length(breaks) - 1,
        prefix = prefix,
        labels = if (keep_na) {
          labels <- c(missing_lab, labels)
        } else {
          labels
        },
        keep_na = keep_na
      )
    } else {
      out <- list(bins = 0)
      cli::cli_warn(
        "Data not binned; too few unique values per bin. Adjust
         {.arg min_unique} as needed."
      )
    }
    class(out) <- "discretize"
    out
  }

#' @rdname discretize
#' @param object An object of class `discretize`.
#' @param new_data A new numeric object to be binned.
#' @export
predict.discretize <- function(object, new_data, ...) {
  if (
    is.matrix(new_data) |
      is.data.frame(new_data)
  ) {
    new_data <- new_data[, 1]
  }
  object$labels <- if (is.null(object$prefix)) {
    object$prefix
  } else {
    paste0(object$prefix, object$labels)
  }
  if (object$bins >= 1) {
    labs <- if (object$keep_na) {
      object$labels[-1]
    } else {
      object$labels
    }
    if (all(is.na(new_data))) {
      out <- factor(new_data, levels = labs)
    } else {
      out <-
        cut(new_data, object$breaks, labels = labs, include.lowest = TRUE)
    }

    if (object$keep_na) {
      out_levels <- levels(out)
      out <- as.character(out)
      if (anyNA(new_data)) {
        missing_label <- object$labels[1] %||% "[missing]"

        out[is.na(new_data)] <- missing_label
        out_levels <- c(missing_label, out_levels)
      }
      out <- factor(out, levels = out_levels)
    }
  } else {
    out <- new_data
  }

  out
}

#' @export
print.discretize <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    if (length(x$breaks) > 0) {
      cat("Bins:", length(x$labels))
      if (any(grepl("_missing", x$labels))) {
        cat(" (includes missing category)")
      }
      cat("\n")

      if (length(x$breaks) <= 6) {
        cat(
          "Breaks:",
          paste(signif(x$breaks, digits = digits), collapse = ", ")
        )
      }
    } else {
      if (x$bins == 0) {
        cat("Too few unique data points. No binning was used.")
      } else {
        cat("Non-numeric data. No binning was used.")
      }
    }
  }

#' Discretize Numeric Variables
#'
#' `step_discretize()` creates a *specification* of a recipe step that will
#' convert numeric data into a factor with bins having approximately the same
#' number of data points (based on a training set).
#'
#' @inheritParams step_center
#' @param num_breaks An integer defining how many cuts to make of the
#'  data.
#' @param min_unique An integer defining a sample size line of
#'  dignity for the binning. If (the number of unique
#'  values)`/(cuts+1)` is less than `min_unique`, no
#'  discretization takes place.
#' @param objects The [discretize()] objects are stored
#'  here once the recipe has be trained by
#'  [prep()].
#' @param options A list of options to [discretize()]. A
#'  default is set for the argument `x`. Note that using
#'  the options `prefix` and `labels` when more than one
#'  variable is being transformed might be problematic as all
#'  variables inherit those values.
#' @template step-return
#' @details
#'
#' Note that missing values will be turned into a factor level with the level
#' `prefix_missing`, where `prefix` is specified in the `options` argument.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the breaks}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_discretize"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @family discretization steps
#' @export
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' ) |>
#'   step_discretize(carbon, hydrogen)
#'
#' rec <- prep(rec, biomass_tr)
#' binned_te <- bake(rec, biomass_te)
#' table(binned_te$carbon)
#'
#' tidy(rec, 1)
step_discretize <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  num_breaks = 4,
  min_unique = 10,
  objects = NULL,
  options = list(prefix = "bin"),
  skip = FALSE,
  id = rand_id("discretize")
) {
  if (any(names(options) == "cuts")) {
    num_breaks <- options$cuts
  }
  if (any(names(options) == "min_unique")) {
    min_unique <- options$min_unique
  }

  add_step(
    recipe,
    step_discretize_new(
      terms = enquos(...),
      trained = trained,
      role = role,
      num_breaks = num_breaks,
      min_unique = min_unique,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_discretize_new <-
  function(
    terms,
    role,
    trained,
    objects,
    num_breaks,
    min_unique,
    options,
    skip,
    id
  ) {
    step(
      subclass = "discretize",
      terms = terms,
      role = role,
      trained = trained,
      num_breaks = num_breaks,
      min_unique = min_unique,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  }

bin_wrapper <- function(x, args) {
  cl <- rlang::call2("discretize", .ns = "recipes", x = x)
  cl <- rlang::call_modify(cl, !!!args)
  rlang::eval_tidy(cl)
}

#' @export
prep.step_discretize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_whole(x$num_breaks, min = 1, arg = "num_breaks")
  check_number_whole(x$min_unique, min = 1, arg = "min_unique")
  check_options(x$options)

  if (
    length(col_names) > 1 & any(names(x$options) %in% c("prefix", "labels"))
  ) {
    cli::cli_warn(
      "Note that the options {.arg prefix} and {.arg labels} will be applied \\
      to all variables."
    )
  }

  x$options$cuts <- x$num_breaks
  x$options$min_unique <- x$min_unique

  obj <- lapply(training[, col_names], bin_wrapper, x$options)
  step_discretize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = obj,
    num_breaks = x$num_breaks,
    min_unique = x$min_unique,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discretize <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- predict(
      object$objects[[col_name]],
      new_data[[col_name]]
    )
  }

  new_data
}

#' @export
print.step_discretize <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Discretize numeric variables from "
    print_step(names(x$objects), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_discretize <- function(x, ...) {
  if (is_trained(x)) {
    brks <- lapply(x$objects, function(x) x$breaks)
    num_brks <- vapply(brks, length, c(1L))
    brk_vars <- rep(names(num_brks), num_brks)

    brks <- unname(brks)
    brks <- lapply(brks, unname)
    values <- vctrs::list_unchop(brks, ptype = double())

    res <- tibble(terms = brk_vars, value = values)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_discretize <- function(x, ...) {
  tibble::tibble(
    name = c("min_unique", "num_breaks"),
    call_info = list(
      list(pkg = "dials", fun = "min_unique"),
      list(pkg = "dials", fun = "num_breaks")
    ),
    source = "recipe",
    component = "step_discretize",
    component_id = x$id
  )
}
