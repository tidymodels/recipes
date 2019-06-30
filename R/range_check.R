#' Check Range Consistency
#'
#' `check_range` creates a *specification* of a recipe
#'  check that will check if the range of a numeric
#'  variable changed in the new data.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the check. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the check be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param slack_prop The allowed slack as a proportion of the range
#'   of the variable in the train set.
#' @param warn If `TRUE` the check will throw a warning instead
#'   of an error when failing.
#' @param lower A named numeric vector of minimum values in the train set.
#'   This is `NULL` until computed by [prep.recipe()].
#' @param upper A named numeric vector of maximum values in the train set.
#'   This is `NULL` until computed by [prep.recipe()].
#' @return An updated version of `recipe` with the new check
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the means).
#' @keywords datagen
#' @concept preprocessing
#' @concept normalization_methods
#' @export
#' @details
#'   The amount of slack that is allowed is determined by the
#'   `slack_prop`. This is a numeric of length one or two. If
#'   of length one, the same proportion will be used at both ends
#'   of the train set range. If of length two, its first value
#'   is used to compute the allowed slack at the lower end,
#'   the second to compute the allowed slack at the upper end.
#' @examples
#'   slack_df <- data_frame(x = 0:100)
#'   slack_new_data <- data_frame(x = -10:110)
#'
#'   # this will fail the check both ends
#' \dontrun{
#'   recipe(slack_df) %>%
#'     check_range(x) %>%
#'     prep() %>%
#'     bake(slack_new_data)
#'  }
#'
#'   # this will fail the check only at the upper end
#' \dontrun{
#'   recipe(slack_df) %>%
#'     check_range(x, slack_prop = c(0.1, 0.05)) %>%
#'     prep() %>%
#'     bake(slack_new_data)
#' }
#'
#'   # give a warning instead of an error
#' \dontrun{
#'   recipe(slack_df) %>%
#'     check_range(x, warn = TRUE) %>%
#'     prep() %>%
#'     bake(slack_new_data)
#' }
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]
check_range <-
  function(recipe,
           ...,
           role       = NA,
           skip       = FALSE,
           trained    = FALSE,
           slack_prop = 0.05,
           warn       = FALSE,
           lower      = NULL,
           upper      = NULL,
           id = rand_id("range_check_")) {
    add_check(
      recipe,
      check_range_new(
        terms   = ellipse_check(...),
        role    = role,
        skip    = skip,
        trained = trained,
        warn    = warn,
        lower   = lower,
        upper   = upper,
        slack_prop = slack_prop,
        id = id
      )
    )
  }

## Initializes a new object
check_range_new <-
  function(terms, role, skip, trained, slack_prop, warn, lower, upper, id) {
    check(
      subclass = "range",
      terms    = terms,
      role     = role,
      skip     = skip,
      trained  = trained,
      warn     = warn,
      lower    = lower,
      upper    = upper,
      slack_prop = slack_prop,
      id       = id
    )
  }

prep.check_range <- function(x,
                             training,
                             info = NULL, # info is an argument to terms_select
                             ...) {
  col_names <- terms_select(x$terms, info = info)

  ## TODO add informative error for nonnumerics

  lower_vals <- vapply(training[ ,col_names], min, c(min = 1),
                       na.rm = TRUE)
  upper_vals <- vapply(training[ ,col_names], max, c(max = 1),
                       na.rm = TRUE)
  check_range_new(
    terms      = x$terms,
    role       = x$role,
    trained    = TRUE,
    skip       = x$skip,
    warn       = x$warn,
    lower      = lower_vals,
    upper      = upper_vals,
    slack_prop = x$slack_prop,
    id         = x$id
  )
}

range_check_func <- function(x,
                             lower,
                             upper,
                             slack_prop = 0.05,
                             warn       = FALSE,
                             colname    = "x") {
  stopifnot(is.numeric(slack_prop),
            is.numeric(x))
  min_x <- min(x)
  max_x <- max(x)
  msg <- NULL
  if (length(slack_prop) == 1) {
    lower_allowed <- lower - ((upper - lower) * slack_prop)
    upper_allowed <- upper + ((upper - lower) * slack_prop)
  } else if (length(slack_prop) == 2) {
    lower_allowed <- lower - ((upper - lower) * slack_prop[1])
    upper_allowed <- upper + ((upper - lower) * slack_prop[2])
  } else {
    stop("slack_prop should be of length 1 or of length 2")
  }

  if (min_x < lower_allowed & max_x > upper_allowed) {
    msg <- paste0("min ", colname, " is ", min_x, ", lower bound is ",
                  lower_allowed,", max x is ", max_x, ", upper bound is ",
                  upper_allowed)
  } else if (min_x < lower_allowed) {
    msg <- paste0("min ", colname, " is ", min_x, ", lower bound is ",
                  lower_allowed)
  } else if (max_x > upper_allowed) {
    msg <- paste0("max ", colname, " is ", max_x, ", upper bound is ",
                  upper_allowed)
  }
  if (warn & !is.null(msg)) {
    warning(msg, call. = FALSE)
  } else if (!is.null(msg)) {
    stop(msg, call. = FALSE)
  }
}

bake.check_range <- function(object,
                             new_data,
                             ...) {

  col_names <- names(object$lower)
  for (i in seq_along(col_names)) {
    colname <- col_names[i]
    range_check_func(new_data[[ colname ]],
                     object$lower[colname],
                     object$upper[colname],
                     object$slack_prop,
                     object$warn,
                     colname)
  }
  as_tibble(new_data)
}

print.check_range <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Checking range of ", sep = "")
    printer(names(x$lower), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname check_range
#' @param x A `check_range` object.
#' @export
tidy.check_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
