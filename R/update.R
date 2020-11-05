#' @importFrom stats update
#' @export
stats::update

#' Update a recipe step
#'
#' This `step` method for `update()` takes named arguments as `...` who's values
#' will replace the elements of the same name in the actual step.
#'
#' For a step to be updated, it must not already have been trained. Otherwise,
#' conflicting information can arise between the data returned from
#' `bake(object, new_data = NULL)` and the information in the step.
#'
#'
#' @param object A recipe `step`.
#' @param ... Key-value pairs where the keys match up with names of elements
#' in the step, and the values are the new values to update the step with.
#'
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' # Create a recipe using step_bs() with degree = 3
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' ) %>%
#'   step_bs(carbon, hydrogen, degree = 3)
#'
#' # Update the step to use degree = 4
#' rec2 <- rec
#' rec2$steps[[1]] <- update(rec2$steps[[1]], degree = 4)
#'
#' # Prep both recipes
#' rec_prepped  <- prep(rec, training = biomass_tr)
#' rec2_prepped <- prep(rec2, training = biomass_tr)
#'
#' # Juice both to see what changed
#' bake(rec_prepped,  new_data = NULL)
#' bake(rec2_prepped, new_data = NULL)
#'
#' # Cannot update a recipe step that has been trained!
#' \dontrun{
#' update(rec_prepped$steps[[1]], degree = 4)
#' }
#'
#' @export
update.step <- function(object, ...) {

  changes <- list(...)

  validate_not_trained(object)

  # Replace the appropriate values in object with the changes
  object <- update_fields(object, changes)

  # Call step() to construct a new step to ensure all new changes are validated
  reconstruct_step(object)

}

update_fields <- function(object, changes) {

  validate_has_unique_names(changes)

  new_nms <- names(changes)
  old_nms <- names(object)

  step_type <- class(object)[1]

  for(nm in new_nms) {

    if (!(nm %in% old_nms)) {
      rlang::abort(glue::glue(
        "The step you are trying to update, ",
        "'{step_type}', does not have the '{nm}' field."
      ))
    }

    object[[nm]] <- changes[[nm]]
  }

  object
}

reconstruct_step <- function(x) {

  # Collect the subclass of the step to use
  # when recreating it
  subclass <- setdiff(class(x), "step")

  # A step is just a list of its arguments
  args <- unclass(x)

  # Construct the call and splice in the args
  # no prefix is needed because we know the full subclass
  call_step <- rlang::call2(
    .fn = "step",
    subclass = subclass,
    !!! args,
    .prefix = "",
    .ns = "recipes"
  )

  rlang::eval_tidy(call_step)

}

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

validate_has_unique_names <- function(x) {
  if (!has_unique_names(x)) {
    rlang::abort("All of the changes supplied in `...` must be uniquely named.")
  }
  invisible(x)
}

validate_not_trained <- function(x) {

  if (is_trained(x)) {

    step_type <- class(x)[1]

    rlang::abort(glue::glue(
      "To update '{step_type}', it must not be trained."
    ))

  }

  invisible(x)

}
