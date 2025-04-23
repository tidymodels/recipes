#' Check variable class
#'
#' `check_class` creates a *specification* of a recipe check that will check if
#' a variable is of a designated class.
#'
#' @inheritParams check_missing
#' @param class_nm A character vector that will be used in `inherits` to check
#'   the class. If `NULL` the classes will be learned in `prep`. Can contain
#'   more than one class.
#' @param allow_additional If `TRUE` a variable is allowed to have additional
#'   classes to the one(s) that are checked.
#' @param class_list A named list of column classes. This is `NULL` until
#'   computed by [prep()].
#' @template check-return
#'
#' @family checks
#' @export
#' @details
#'
#' This function can check the classes of the variables in two ways. When the
#' `class` argument is provided it will check if all the variables specified are
#' of the given class. If this argument is `NULL`, the check will learn the
#' classes of each of the specified variables in [prep()]. Both ways will break
#' [bake()] if the variables are not of the requested class. If a variable has
#' multiple classes in [prep()], all the classes are checked. Please note that
#' in [prep()] the argument `strings_as_factors` defaults to `TRUE`. If the
#' train set contains character variables the check will be break [bake()] when
#' `strings_as_factors` is `TRUE`.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this check, a tibble with columns
#'  `terms` (the selectors or variables selected) and `value` (the type)
#'  is returned.
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(dplyr)
#' data(Sacramento, package = "modeldata")
#'
#' # Learn the classes on the train set
#' train <- Sacramento[1:500, ]
#' test <- Sacramento[501:nrow(Sacramento), ]
#' recipe(train, sqft ~ .) |>
#'   check_class(everything()) |>
#'   prep(train, strings_as_factors = FALSE) |>
#'   bake(test)
#'
#' # Manual specification
#' recipe(train, sqft ~ .) |>
#'   check_class(sqft, class_nm = "integer") |>
#'   check_class(city, zip, type, class_nm = "factor") |>
#'   check_class(latitude, longitude, class_nm = "numeric") |>
#'   prep(train, strings_as_factors = FALSE) |>
#'   bake(test)
#'
#' # By default only the classes that are specified
#' #   are allowed.
#' x_df <- tibble(time = c(Sys.time() - 60, Sys.time()))
#' x_df$time |> class()
#' \dontrun{
#' recipe(x_df) |>
#'   check_class(time, class_nm = "POSIXt") |>
#'   prep(x_df) |>
#'   bake_(x_df)
#' }
#'
#' # Use allow_additional = TRUE if you are fine with it
#' recipe(x_df) |>
#'   check_class(time, class_nm = "POSIXt", allow_additional = TRUE) |>
#'   prep(x_df) |>
#'   bake(x_df)
check_class <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    class_nm = NULL,
    allow_additional = FALSE,
    skip = FALSE,
    class_list = NULL,
    id = rand_id("class")
  ) {
    add_check(
      recipe,
      check_class_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        class_nm = class_nm,
        allow_additional = allow_additional,
        class_list = class_list,
        skip = skip,
        id = id
      )
    )
  }

## Initializes a new object
check_class_new <-
  function(
    terms,
    role,
    trained,
    class_nm,
    allow_additional,
    class_list,
    skip,
    id
  ) {
    check_character(class_nm, allow_null = TRUE, call = rlang::caller_env(2))
    check_bool(allow_additional, call = rlang::caller_env(2))
    check(
      subclass = "class",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      class_nm = class_nm,
      allow_additional = allow_additional,
      class_list = class_list,
      skip = skip,
      id = id
    )
  }

#' @export
prep.check_class <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  # vapply requires a very specific return here
  # class can give back multiple values, return shape
  # is not predetermined. Thats why we use lapply instead.
  if (is.null(x$class_nm)) {
    class_list <- lapply(training[, col_names], class)
  } else {
    class_list <- rep(list(x$class_nm), length(col_names))
    names(class_list) <- col_names
  }

  check_class_new(
    terms = x$terms,
    role = x$role,
    skip = x$skip,
    trained = TRUE,
    class_nm = x$class_nm,
    allow_additional = x$allow_additional,
    class_list = class_list,
    id = x$id
  )
}

# we don't use inherits() because class_nm
# can be of length > 1. inherits will result
# in TRUE if just one of the classes in class_nm
# is present in x.
bake_check_class_core <- function(x, class_nm, var_nm, aa = FALSE) {
  classes <- class(x)
  missing <- setdiff(class_nm, classes)
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.var {var_nm}} should have the class{?es} {.cls {class_nm}} but
      has the class{?es} {.and {.cls {classes}}}."
    )
  }

  extra <- setdiff(classes, class_nm)
  if (length(extra) > 0 && !aa) {
    cli::cli_abort(
      c(
        x = "{.var {var_nm}} has class{?es} {.and {.cls {classes}}} but only \\
            the following {?is/are} asked: {.and {.cls {class_nm}}}.",
        i = "This error is shown because {.arg allow_additional} is set to \\
            {.val FALSE}."
      )
    )
  }
}

#' @export
bake.check_class <- function(object, new_data, ...) {
  col_names <- names(object$class_list)
  check_new_data(col_names, object, new_data)

  mapply(
    bake_check_class_core,
    new_data[, col_names],
    object$class_list,
    col_names,
    MoreArgs = list(aa = object$allow_additional)
  )

  new_data
}

#' @export
print.check_class <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Checking the class(es) for "
    print_step(names(x$class_list), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_class <- function(x, ...) {
  if (is_trained(x)) {
    values <- vapply(
      unname(x$class_list),
      FUN = function(x) paste0(x, collapse = "-"),
      FUN.VALUE = character(1)
    )
    res <- tibble(terms = names(x$class_list), value = values)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_chr)
  }
  res$id <- x$id
  res
}
