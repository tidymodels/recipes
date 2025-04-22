#' Prototype of recipe object
#'
#' This helper function returns the prototype of the input data set expected by
#' the recipe object.
#'
#' @param x A `recipe` object.
#' @param ... currently not used.
#' @param stage A single character. Must be one of `"prep"` or `"bake"`. See
#'   details for more. Defaults to `"prep"`.
#'
#' @details
#'
#' The returned ptype is a tibble of the data set that the recipe object is
#' expecting. The specifics of which columns depend on the `stage`.
#'
#' At [prep()] time, when `stage = "prep"`, the ptype is the data passed to
#' [recipe()]. The following code chunk represents a possible recipe scenario.
#' `recipes_ptype(rec_spec, stage = "prep")` and `recipes_ptype(rec_prep, stage
#' = "prep")` both return a ptype tibble corresponding to `data_ptype`. This
#' information is used internally in [prep()] to verify that `data_training` has
#' the right columns with the right types.
#'
#' ```r
#' rec_spec <- recipe(outcome ~ ., data = data_ptype) |>
#'   step_normalize(all_numeric_predictors()) |>
#'   step_dummy(all_nominal_predictors())
#'
#' rec_prep <- prep(rec_spec, training = data_training)
#' ```
#'
#' At [bake()] time, when `stage = "bake"`, the ptype represents the data that
#' are required for [bake()] to run.
#'
#' ```r
#' data_bake <- bake(rec_prep, new_data = data_testing)
#' ```
#'
#' What this means in practice is that unless otherwise specified, everything
#' but outcomes and case weights are required. These requirements can be changed
#' with [update_role_requirements()], and `recipes_ptype()` respects those
#' changes.
#'
#' `recipes_ptype()` returns `NULL` on recipes created prior to version 1.1.0.
#'
#' Note that the order of the columns aren't guaranteed to align with
#' `data_ptype` as the data internally is ordered according to roles.
#'
#' @return A zero row tibble.
#' @keywords internal
#'
#' @seealso [developer_functions] [recipes_ptype_validate]
#'
#' @examples
#' training <- tibble(
#'   y = 1:10,
#'   id = 1:10,
#'   x1 = letters[1:10],
#'   x2 = factor(letters[1:10]),
#'   cw = hardhat::importance_weights(1:10)
#' )
#' training
#'
#' rec_spec <- recipe(y ~ ., data = training)
#'
#' # outcomes and case_weights are not required at bake time
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#'
#' rec_spec <- recipe(y ~ ., data = training) |>
#'   update_role(x1, new_role = "id")
#'
#' # outcomes and case_weights are not required at bake time
#' # "id" column is assumed to be needed
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#'
#' rec_spec <- recipe(y ~ ., data = training) |>
#'   update_role(x1, new_role = "id") |>
#'   update_role_requirements("id", bake = FALSE)
#'
#' # update_role_requirements() is used to specify that "id" isn't needed
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#'
#' @export
recipes_ptype <- function(x, ..., stage = "prep") {
  check_dots_empty0(...)

  # recipe created prior to 1.1.0
  if (is.null(x$ptype)) {
    return(NULL)
  }

  ptype <- x$ptype

  stage <- rlang::arg_match(stage, values = c("prep", "bake"))

  if (stage == "bake") {
    required_roles <- compute_bake_role_requirements(x)

    var_info <- x$var_info
    roles <- var_info$role
    roles <- chr_explicit_na(roles)

    required_var <- var_info$variable[required_roles[roles]]

    ptype <- ptype[names(ptype) %in% required_var]
  }

  ptype
}

long_function_name <- function(x, ..., verbose = FALSE) {
  x
}

#' Validate prototype of recipe object
#'
#' This helper function validates a dataframe against the ptype of a recipe.
#'
#' @param x A `recipe` object.
#' @param new_data A data.frame. To be patched aganist ptype of `x`.
#' @param ... currently not used.
#' @param stage A single character. Must be one of `"prep"` or `"bake"`. See
#'   details for more. Defaults to `"prep"`.
#' @param call The execution environment of a currently running function, e.g.
#'   `caller_env()`. The function will be mentioned in error messages as the
#'   source of the error. See the call argument of [rlang::abort()] for more
#'   information.
#'
#' @return Nothing or an error.
#' @keywords internal
#'
#' @seealso [developer_functions] [recipes_ptype]
#'
#' @examples
#' rec <- recipe(mpg ~ disp, data = mtcars)
#'
#' recipes_ptype_validate(rec, mtcars)
#'
#' @export
recipes_ptype_validate <- function(
  x,
  new_data,
  ...,
  stage = "prep",
  call = rlang::caller_env()
) {
  old_ptype <- recipes_ptype(x, stage = stage)

  # recipe created prior to 1.1.0
  if (is.null(old_ptype)) {
    return(invisible())
  }

  col_names <- names(old_ptype)

  new_ptype <- vctrs::vec_ptype(new_data)

  if (!all(col_names %in% names(new_ptype))) {
    offenders <- col_names[!col_names %in% names(new_ptype)]
    cli::cli_abort(
      "Not all variables in the recipe are present in the supplied training \\
      set: {.var {offenders}}.",
      call = call
    )
  }

  new_ptype <- new_ptype[col_names]

  if (!identical(lapply(old_ptype, class), lapply(new_ptype, class))) {
    old_classes <- lapply(old_ptype, class)
    new_classes <- lapply(new_ptype, class)

    offenders <- purrr::map2_lgl(old_classes, new_classes, identical)
    offenders <- col_names[!offenders]

    msg <- c(
      "x" = "{cli::qty(offenders)} The following variable{?s} \\
                   {?has/have} the wrong class:"
    )

    # Use `paste0()` rather than typical glue syntax to intentionally duplicate messages
    col_msg <- paste0(
      "{.var {offenders[",
      seq_along(offenders),
      "]}} must have class {.cls {new_classes[offenders[",
      seq_along(offenders),
      "]]}}, not {.cls {old_classes[offenders[",
      seq_along(offenders),
      "]]}}."
    )
    names(col_msg) <- rep("*", length(col_msg))
    msg <- c(msg, col_msg)

    cli::cli_abort(msg, call = call)
  }

  if (
    !identical(lapply(old_ptype, attributes), lapply(new_ptype, attributes))
  ) {
    old_attributes <- lapply(old_ptype, attributes)
    new_attributes <- lapply(new_ptype, attributes)

    offenders <- purrr::map2_lgl(old_attributes, new_attributes, identical)
    offenders <- col_names[!offenders]

    msg <- c(
      "x" = "{cli::qty(offenders)} The following variable{?s} \\
    {?has/have} the wrong attributes: {.var {offenders}}."
    )

    if (any(map_lgl(old_ptype[offenders], is.factor))) {
      offenders_fct <- map_lgl(old_ptype[offenders], is.factor)
      offenders_fct <- names(offenders_fct)

      fct_msg <- c(
        "*" = "The factor levels of {.var {offenders_fct}} don't match."
      )
      msg <- c(msg, fct_msg)
    }

    msg <- c(
      msg,
      "Run {.code lapply(recipes_ptype(rec), attributes)} to see expected \\
      attributes. For {.code rec} being the name of your recipe."
    )

    cli::cli_abort(msg, call = call)
  }

  invisible()
}
