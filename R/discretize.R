#' Discretize Numeric Variables
#'
#' \code{discretize} converts a numeric vector into a factor with bins having
#'   approximately the same number of data points (based on a training set).
#'
#' @export
#' @param x A numeric vector
discretize <- function(x, ...)
  UseMethod("discretize")

#' @rdname discretize
discretize.default <- function(x, ...)
  stop("Only numeric `x` is accepted")

#' @rdname discretize
#' @param cuts An integer defining how many cuts to make of the data.
#' @param labels A character vector defining the factor levels that will be in
#' the new factor (from smallest to largest). This should have length
#'  \code{cuts+1} and should not include a level for missing (see
#'  \code{keep_na} below).
#' @param prefix A single parameter value to be used as a prefix for the factor
#'   levels (e.g. \code{bin1}, \code{bin2}, ...). If the string is not a valid
#'   R name, it is coerced to one.
#' @param keep_na A logical for whether a factor level should be created to
#'   identify missing values in \code{x}.
#' @param infs A logical indicating whether the smallest and largest cut point
#'   should be infinite.
#' @param min_unique An integer defining a sample size line of dignity for the
#'   binning. If (the number of unique values)\code{/(cuts+1)} is less than
#'   \code{min_unique}, no discretization takes place.
#' @param ... For \code{discretize}: options to pass to
#'   \code{\link[stats]{quantile}} that should not include \code{x} or
#'   \code{probs}. For \code{step_discretize}, the dots specify one or more
#'   selector functions to choose which variables are affected by the step. See
#'   \code{\link{selections}} for more details.
#'
#' @return \code{discretize} returns an object of class \code{discretize}.
#'   \code{predict.discretize} returns a factor vector.
#' @keywords datagen
#' @concept preprocessing discretization factors
#' @export
#' @details \code{discretize} estimates the cut points from \code{x} using
#'   percentiles. For example, if \code{cuts = 3}, the function estimates the
#'  quartiles of \code{x} and uses these as the cut points. If \code{cuts = 2},
#'  the bins are defined as being above or below the median of \code{x}.
#'
#' The \code{predict} method can then be used to turn numeric vectors into
#'  factor vectors.
#'
#' If \code{keep_na = TRUE}, a suffix of "_missing" is used as a factor level
#'  (see the examples below).
#'
#'If \code{infs = FALSE} and a new value is greater than the largest value of
#'  \code{x}, a missing value will result.
#'@examples
#'data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
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
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#' rec <- rec %>% step_discretize(carbon, hydrogen)
#' rec <- prep(rec, biomass_tr)
#' binned_te <- bake(rec, biomass_te)
#' table(binned_te$carbon)

#' @importFrom stats quantile

discretize.numeric <-
  function(x,
           cuts = 4,
           labels = NULL,
           prefix = "bin",
           keep_na = TRUE,
           infs = TRUE,
           min_unique = 10,
           ...) {
    unique_vals <- length(unique(x))
    missing_lab <- "_missing"
    
    if (cuts < 2)
      stop("There should be at least 2 cuts")
    
    if (unique_vals / (cuts + 1) >= min_unique) {
      breaks <- quantile(x, probs = seq(0, 1, length = cuts + 1), ...)
      num_breaks <- length(breaks)
      breaks <- unique(breaks)
      if (num_breaks > length(breaks))
        warning(
          "Not enough data for ",
          cuts,
          " breaks. Only ",
          length(breaks),
          " breaks were used.",
          sep = ""
        )
      if (infs) {
        breaks[1] <- -Inf
        breaks[length(breaks)] <- Inf
      }
      breaks <- unique(breaks)
      
      if (is.null(labels)) {
        prefix <- prefix[1]
        if (make.names(prefix) != prefix) {
          warning(
            "The prefix '",
            prefix,
            "' is not a valid R name. It has been changed to '",
            make.names(prefix),
            "'."
          )
          prefix <- make.names(prefix)
        }
        labels <- names0(length(breaks) - 1, "")
      }
      out <- list(
        breaks = breaks,
        bins = length(breaks) - 1,
        prefix = prefix,
        labels =  if (keep_na)
          labels <- c(missing_lab, labels)
        else
          labels,
        keep_na = keep_na
      )
    } else {
      out <- list(bins = 0)
      warning("Data not binned; too few unique values per bin. ",
              "Adjust 'min_unique' as needed", call. = FALSE)
    }
    class(out) <- "discretize"
    out
  }

#' @rdname discretize
#' @importFrom stats predict
#' @param object An object of class \code{discretize}.
#' @param newdata A new numeric object to be binned.
#' @export
predict.discretize <- function(object, newdata, ...) {
  if (is.matrix(newdata) |
      is.data.frame(newdata))
    newdata <- newdata[, 1]
  object$labels <- paste0(object$prefix, object$labels)
  if (object$bins >= 1) {
    labs <- if (object$keep_na)
      object$labels[-1]
    else
      object$labels
    out <-
      cut(newdata,
          object$breaks,
          labels = labs,
          include.lowest = TRUE)
    if (object$keep_na) {
      out <- as.character(out)
      if (any(is.na(newdata)))
        out[is.na(newdata)] <- object$labels[1]
      out <- factor(out, levels = object$labels)
    }
  } else
    out <- newdata
  
  out
}

#' @export
print.discretize <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    if (length(x$breaks) > 0) {
      cat("Bins:", length(x$labels))
      if (any(grepl("_missing", x$labels)))
        cat(" (includes missing category)")
      cat("\n")
      
      if (length(x$breaks) <= 6) {
        cat("Breaks:",
            paste(signif(x$breaks, digits = digits), collapse = ", "))
      }
    } else {
      if (x$bins == 0)
        cat("Too few unique data points. No binning.")
      else
        cat("Non-numeric data. No binning was used.")
    }
  }


#' @rdname discretize
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param objects The \code{\link{discretize}} objects are stored here once
#'   the recipe has be trained by \code{\link{prep.recipe}}.
#' @param options A list of options to \code{\link{discretize}}. A defaults is
#'   set for the argument \code{x}. Note that the using the options
#'   \code{prefix} and \code{labels} when more than one variable is being
#'   transformed might be problematic as all variables inherit those values.
#' @export

step_discretize <- function(recipe,
                            ...,
                            role = NA,
                            trained = FALSE,
                            objects = NULL,
                            options = list()) {
  add_step(
    recipe,
    step_discretize_new(
      terms = check_ellipses(...),
      trained = trained,
      role = role,
      objects = objects,
      options = options
    )
  )
}

step_discretize_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           objects = NULL,
           options = NULL) {
    step(
      subclass = "discretize",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      options = options
    )
  }

bin_wrapper <- function(x, args) {
  bin_call <-
    quote(discretize(x, cuts, labels, prefix, keep_na, infs, min_unique, ...))
  args <- sub_args(discretize.numeric, args, "x")
  args$x <- x
  eval(bin_call, envir = args)
}

#' @export
prep.step_discretize <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (length(col_names) > 1 &
      any(names(x$options) %in% c("prefix", "labels"))) {
    warning("Note that the options `prefix` and `labels`",
            "will be applied to all variables")
  }
  
  obj <- lapply(training[, col_names], bin_wrapper, x$options)
  step_discretize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = obj,
    options = x$options
  )
}

#' @importFrom tibble as_tibble
#' @importFrom stats predict
#' @export
bake.step_discretize <- function(object, newdata, ...) {
  for (i in names(object$objects))
    newdata[, i] <-
      predict(object$objects[[i]], getElement(newdata, i))
  as_tibble(newdata)
}

print.step_discretize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Dummy variables from ")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }
