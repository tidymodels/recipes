## Overall wrappers to make new `step_X` or `check_Y` objects
#'
#' `step()` sets the class of the `step` and `check()` is for checks.
#'
#' @param subclass A character string for the resulting class. For example, if
#'   `subclass = "blah"` the step object that is returned has class `step_blah`
#'   or `check_blah` depending on the context.
#' @param ... All arguments to the operator that should be returned. Required
#'   arguments are `trained`, `skip`, and `id`.
#' @param .prefix Prefix to the subclass created.
#'
#' @seealso [developer_functions]
#'
#' @keywords internal
#' @return An updated step or check with the new class.
#' @export
step <- function(
  subclass,
  ...,
  .prefix = "step_",
  call = rlang::caller_env(2)
) {
  args <- list(...)

  check_string(subclass, call = call)
  .prefix <- rlang::arg_match0(
    .prefix,
    c("step_", "check_", ""),
    error_call = call
  )
  check_step_check_args(args, call = call)

  structure(args, class = c(paste0(.prefix, subclass), "step"))
}

#' @rdname step
#' @export
check <- function(
  subclass,
  ...,
  .prefix = "check_",
  call = rlang::caller_env(2)
) {
  args <- list(...)

  check_string(subclass, call = call)
  .prefix <- rlang::arg_match0(
    .prefix,
    c("step_", "check_", ""),
    error_call = call
  )
  check_step_check_args(args, call = call)

  structure(args, class = c(paste0(.prefix, subclass), "check"))
}

#' Add a New Operation to the Current Recipe
#'
#' `add_step()` adds a step to the last location in the recipe. `add_check()`
#' does the same for checks.
#'
#' @param rec A [recipe()].
#' @param object A step or check object.
#' @return A updated [recipe()] with the new operation in the last slot.
#'
#' @seealso [developer_functions]
#'
#' @export
add_step <- function(rec, object) {
  rec$steps[[length(rec$steps) + 1]] <- object
  rec
}

#' @rdname add_step
#' @export
add_check <- function(rec, object) {
  rec$steps[[length(rec$steps) + 1]] <- object
  rec
}

# ------------------------------------------------------------------------------

check_step_check_args <- function(x, call = rlang::caller_env()) {
  req_args <- c("trained", "id", "skip", "role")
  nms <- names(x)
  has_req_args <- req_args %in% nms
  if (!all(has_req_args)) {
    miss_args <- req_args[!has_req_args]
    cli::cli_abort(
      "Some required arguments are missing: {.arg {miss_args}}.",
      call = call
    )
  }
  check_bool(x$trained, call = call, arg = "trained")
  check_bool(x$skip, call = call, arg = "skip")
  check_string(x$id, call = call, arg = "id")
  check_string(x$role, allow_empty = FALSE, allow_na = TRUE, call = call)

  if (any(names(x) == "keep_original_cols")) {
    check_bool(x$keep_original_cols, call = call, arg = "keep_original_cols")
  }

  invisible(x)
}
