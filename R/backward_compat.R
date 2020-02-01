fun_from_class <- function(object) {
  fun <- rlang::env_get(
    nm = class(object)[[1]],
    default = NULL,
    inherit = TRUE
  )
  if (rlang::is_function(fun)) {
    fun
  } else {
    rlang::abort(
      paste(
        class(object)[[1]], "is not a function.\n",
        "Please specify the step function."
      )
    )
  }
}

#' Add New Arguments
#'
#' Add the defaults of newly defined arguments to a step before prepping or
#' baking.
#'
#' @param object The (step) object to modify.
#' @param fun The function that was used to create the step. This is inferred
#'   from the class.
#' @param args The arguments to modify. These are inferred from `fun` when
#'   possible.
#'
#' @return `object` with new argument defaults added.
#' @keywords internal
add_new_args <- function(object,
                         fun = fun_from_class(object),
                         args = formals(fun)) {
  to_ignore <- c("...", "recipe", "role", "trained", "skip", "prefix", "id")
  args <- args[!(names(args) %in% to_ignore)]

  object[setdiff(names(args), names(object))] <-
    args[setdiff(names(args), names(object))]
}
