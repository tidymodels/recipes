#' Manually Alter Roles
#'
#' @description
#' `add_role()` adds a _new_ role to an existing variable in the recipe. It
#' does not overwrite old roles, as a single variable can have multiple roles.
#'
#' `update_role()` alters an existing role in the recipe.
#'
#' @param recipe An existing [recipe()].
#'
#' @param ... One or more selector functions to choose which variables are
#'   being assigned a role. See [selections()] for more details.
#'
#' @param new_role A character string for a single role.
#'
#' @param new_type A character string for specific type that the variable should
#' be identified as. If left as `NULL`, the type is automatically identified
#' as the _first_ type you see for that variable in `summary(recipe)`.
#'
#' @param old_role A character string for the specific role to update for the
#' variables selected by `...`. If left as `NULL`, all roles for each selected
#' variable are updated to `new_role`. This is useful if you have a
#' variable with multiple roles, and you want to alter only one of them.
#'
#' @return An updated recipe object.
#'
#' @details
#'
#' With `add_role()`, if a variable is selected that already has the
#' `new_role`, a warning is emitted and that variable is skipped so no
#' duplicate roles are added.
#'
#' Adding or updating roles is a useful way to group certain variables that
#' don't fall in the standard `"predictor"` bucket. You can perform a step
#' on all of the variables that have a custom role with the selector
#' [has_role()].
#'
#' @keywords datagen
#' @concept preprocessing model_specification
#' @examples
#' data(biomass)
#'
#' # Create the recipe manually
#' rec <- recipe(x = biomass)
#' rec
#' summary(rec)
#'
#' rec <- rec %>%
#'   update_role(carbon, contains("gen"), sulfur, new_role = "predictor") %>%
#'   update_role(sample, new_role = "id variable") %>%
#'   update_role(dataset, new_role = "splitting variable") %>%
#'   update_role(HHV, new_role = "outcome")
#'
#' rec
#' summary(rec)
#'
#' # Add a secondary role for carbon
#' rec <- rec %>%
#'   add_role(carbon, new_role = "carbon predictor")
#'
#' summary(rec)
#'
#' # Now update only the "predictor" role of carbon to instead
#' # be an additional outcome
#' rec %>%
#'   update_role(carbon, new_role = "outcome", old_role = "predictor") %>%
#'   summary()
#'
#' @importFrom rlang quos
#' @name roles
NULL

#' @export
#' @rdname roles
add_role <- function(recipe, ..., new_role = "predictor", new_type = NULL) {

  if (length(new_role) != 1L) {
    stop("`new_role` must have length 1.")
  }

  if (is.na(new_role)) {
    new_role <- as.character(new_role)
  }

  if (!is.character(new_role)) {
    stop("`new_role` must be a character vector.")
  }

  if (length(new_type) != 1L & length(new_type) != 0L) {
    stop("`new_type` must have length 1.")
  }

  if (!is.character(new_type) & !is.null(new_type)) {
    stop("`new_type` must be a character vector, or `NULL`.")
  }

  terms <- quos(...)

  if (is_empty(terms)) {
    warning("No selectors were found", call. = FALSE)
  }

  vars <- terms_select(terms = terms, info = summary(recipe))

  # Check to see if role already exists
  # remove variables where the role already exists
  existing_var_idx <- recipe$var_info$variable %in% vars
  role_already_exists <- recipe$var_info$role[existing_var_idx] %in% new_role

  # Allow the user to add the same role with a different type
  if (!is.null(new_type)) {
    type_already_exists <- recipe$var_info$type[existing_var_idx] %in% new_type
    role_already_exists <- role_already_exists & type_already_exists
  }

  if (any(role_already_exists)) {
    existing_vars <- recipe$var_info$variable[existing_var_idx]
    vars_that_role_exists_for <- existing_vars[role_already_exists]

    warning(
      "Role `", new_role, "` already exists for ",
      paste0(vars_that_role_exists_for, collapse = ", "),
      ". Skipping.",
      call. = FALSE
    )

    vars <- vars[!(vars %in% vars_that_role_exists_for)]
  }

  # Pull in first type we come across if unspecified
  if (is.null(new_type)) {
    new_type <- purrr::map_chr(vars, ~{
      first_row_with_var <- which(recipe$var_info$variable == .x)[1]
      recipe$var_info$type[first_row_with_var]
    })
  } else {
    new_type <- rep(new_type, times = length(vars))
  }

  source <- purrr::map_chr(vars, ~{
    first_row_with_var <- which(recipe$var_info$variable == .x)[1]
    recipe$var_info$source[first_row_with_var]
  })

  for(i in seq_along(vars)) {
    last_row_with_var <- dplyr::last(which(recipe$var_info$variable == vars[i]))
    recipe$var_info <- tibble::add_row(
      .data = recipe$var_info,
      variable = vars[i],
      type = new_type[i],
      role = new_role,
      source = source[i],
      .after = last_row_with_var
    )
  }

  recipe$term_info <- recipe$var_info
  recipe

}

#' @export
#' @rdname roles
update_role <- function(recipe, ..., new_role = "predictor", old_role = NULL) {

  if (length(new_role) != 1L) {
    stop("`new_role` must have length 1.")
  }

  if (is.na(new_role)) {
    new_role <- as.character(new_role)
  }

  if (!is.character(new_role)) {
    stop("`new_role` must be a character vector.")
  }

  if (length(old_role) != 1L & length(old_role) != 0L) {
    stop("`old_role` must have length 1.")
  }

  if (!is.null(old_role) && is.na(old_role)) {
    old_role <- as.character(old_role)
  }

  if (!is.character(old_role) & !is.null(old_role)) {
    stop("`old_role` must be a character vector, or `NULL`.")
  }

  terms <- quos(...)

  if (is_empty(terms)) {
    warning("No selectors were found", call. = FALSE)
  }

  vars <- terms_select(terms = terms, info = summary(recipe))

  rows_to_update <- recipe$var_info$variable %in% vars

  if (!is.null(old_role)) {
    rows_to_update <- rows_to_update & (recipe$var_info$role %in% old_role)
  }

  recipe$var_info$role[rows_to_update] <- new_role

  recipe$term_info <- recipe$var_info
  recipe
}
