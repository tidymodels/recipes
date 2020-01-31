#' Manually Alter Roles
#'
#' @description
#' `update_role()` alters an existing role in the recipe or assigns an initial
#' role to variables that do not yet have a declared role.
#'
#' `add_role()` adds an _additional_ role to variables that already have a role
#' in the recipe. It does not overwrite old roles, as a single variable can have
#' multiple roles.
#'
#' `remove_role()` eliminates a single existing role in the recipe.
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
#' variables selected by `...`. `update_role()` accepts a `NULL` as long as the
#' variables have only a single role.
#'
#' @return An updated recipe object.
#'
#' @details
#'
#' `update_role()` should be used when a variable doesn't currently have a role
#' in the recipe, or to replace an `old_role` with a `new_role`. `add_role()`
#' only adds additional roles to variables that already have roles and will
#' throw an error when the current role is missing (i.e. `NA`).
#'
#' When using `add_role()`, if a variable is selected that already has the
#' `new_role`, a warning is emitted and that variable is skipped so no duplicate
#' roles are added.
#'
#' Adding or updating roles is a useful way to group certain variables that
#' don't fall in the standard `"predictor"` bucket. You can perform a step
#' on all of the variables that have a custom role with the selector
#' [has_role()].
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept model_specification
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(biomass)
#'
#' # Using the formula method, roles are created for any outcomes and predictors:
#' recipe(HHV ~ ., data = biomass) %>%
#'   summary()
#'
#' # However `sample` and `dataset` aren't predictors. Since they already have
#' # roles, `update_role()` can be used to make changes:
#' recipe(HHV ~ ., data = biomass) %>%
#'   update_role(sample, new_role = "id variable") %>%
#'   update_role(dataset, new_role = "splitting variable") %>%
#'   summary()
#'
#' # `update_role()` cannot set a role to NA, use `remove_role()` for that
#' \dontrun{
#' recipe(HHV ~ ., data = biomass) %>%
#'   update_role(sample, new_role = NA_character_)
#' }
#'
#' # ------------------------------------------------------------------------------
#'
#' # Variables can have more than one role. `add_role()` can be used
#' # if the column already has at least one role:
#' recipe(HHV ~ ., data = biomass) %>%
#'   add_role(carbon, sulfur, new_role = "something") %>%
#'   summary()
#'
#' # `update_role()` has an argument called `old_role` that is required to
#' # unambiguously update a role when the column currently has multiple roles.
#' recipe(HHV ~ ., data = biomass) %>%
#'   add_role(carbon, new_role = "something") %>%
#'   update_role(carbon, new_role = "something else", old_role = "something") %>%
#'   summary()
#'
#' # `carbon` has two roles at the end, so the last `update_roles()` fails since
#' # `old_role` was not given.
#' \dontrun{
#' recipe(HHV ~ ., data = biomass) %>%
#'   add_role(carbon, sulfur, new_role = "something") %>%
#'   update_role(carbon, new_role = "something else")
#' }
#'
#' # ------------------------------------------------------------------------------
#'
#' # To remove a role, `remove_role()` can be used to remove a single role.
#' recipe(HHV ~ ., data = biomass) %>%
#'   add_role(carbon, new_role = "something") %>%
#'   remove_role(carbon, old_role = "something") %>%
#'   summary()
#'
#' # To remove all roles, call `remove_role()` multiple times to reset to `NA`
#' recipe(HHV ~ ., data = biomass) %>%
#'   add_role(carbon, new_role = "something") %>%
#'   remove_role(carbon, old_role = "something") %>%
#'   remove_role(carbon, old_role = "predictor") %>%
#'   summary()
#'
#' # ------------------------------------------------------------------------------
#'
#' # If the formula method is not used, all columns have a missing role:
#' recipe(biomass) %>%
#'   summary()
#'
#' @name roles
NULL

#' @export
#' @rdname roles
add_role <- function(recipe, ..., new_role = "predictor", new_type = NULL) {
  single_chr(new_role, "new_", null_ok = FALSE)

  if (length(new_type) != 1L & length(new_type) != 0L) {
    rlang::abort("`new_type` must have length 1.")
  }

  if (!is.character(new_type) & !is.null(new_type)) {
    rlang::abort("`new_type` must be a character vector, or `NULL`.")
  }

  terms <- quos(...)

  if (is_empty(terms)) {
    rlang::warn("No selectors were found")
  }

  vars <- terms_select(terms = terms, info = summary(recipe))

  # Check to see if role already exists
  # remove variables where the role already exists
  existing_var_idx <- recipe$var_info$variable %in% vars

  if (all(is.na(recipe$var_info$role[existing_var_idx]))) {
    vars <- glue::glue_collapse(glue::single_quote(vars), sep = ", ")
    rlang::abort(glue::glue(
      "No role currently exists for column(s): {vars}. Please use ",
      "`update_role()` instead."
    ))
  }

  role_already_exists <- recipe$var_info$role[existing_var_idx] %in% new_role

  # Allow the user to add the same role with a different type
  if (!is.null(new_type)) {
    type_already_exists <- recipe$var_info$type[existing_var_idx] %in% new_type
    role_already_exists <- role_already_exists & type_already_exists
  }

  if (any(role_already_exists)) {
    existing_vars <- recipe$var_info$variable[existing_var_idx]
    vars_that_role_exists_for <- existing_vars[role_already_exists]

    bad_vars <- glue::glue_collapse(
      glue::single_quote(vars_that_role_exists_for),
      sep = ", "
    )

    rlang::warn(
      glue::glue(
        "Role, '{new_role}', already exists for column(s): {bad_vars}. ",
        "Skipping."
      )
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

  for (i in seq_along(vars)) {
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
  single_chr(new_role, "new_", null_ok = FALSE)
  single_chr(old_role, "old_", null_ok = TRUE)

  terms <- quos(...)

  if (is_empty(terms)) {
    rlang::warn("No selectors were found")
  }

  rec_vars <- summary(recipe)
  vars <- terms_select(terms = terms, info = rec_vars)

  # check to see if any variables have multiple roles
  if (is.null(old_role)) {
    var_counts <-
      rec_vars %>%
      dplyr::filter(variable %in% vars) %>%
      dplyr::group_by(variable) %>%
      dplyr::count()
    if (any(var_counts$n > 1)) {
      rlang::abort(
        paste0(
          "`old_role` can only be `NULL` when the variable(s) have ",
          "a single existing role."
        )
      )
    }
  }

  rows_to_update <- recipe$var_info$variable %in% vars

  if (!is.null(old_role)) {
    rows_to_update <- rows_to_update & (recipe$var_info$role %in% old_role)
  }

  recipe$var_info$role[rows_to_update] <- new_role

  recipe$term_info <- recipe$var_info
  recipe
}

# ------------------------------------------------------------------------------

#' @rdname roles
#' @export
remove_role <- function(recipe, ..., old_role) {
  single_chr(old_role, "old_")

  terms <- quos(...)
  if (is_empty(terms)) {
    rlang::warn("No selectors were found")
  }
  vars <- terms_select(terms = terms, info = summary(recipe))
  if (length(vars) == 0) {
    rlang::warn("No columns were selected for role removal.")
  }

  term_info <- summary(recipe)

  term_info <-
    term_info %>%
    mutate(.orig_order = 1:nrow(term_info)) %>%
    group_by(variable) %>%
    do(role_rm_machine(., role = old_role, var = vars)) %>%
    ungroup() %>%
    arrange(.orig_order) %>%
    dplyr::select(-.orig_order)

  recipe$var_info <- term_info
  recipe$term_info <- recipe$var_info
  recipe
}

# Does anyone remember the 80's band The Cult?
role_rm_machine <- function(x, role, var) {
  if (!any(x$variable %in% var)) {
    return(x)
  }

  sel_role <- x$role == role

  if (sum(sel_role) == 0) {
    var <- glue::single_quote(x$variable[1])
    role <- glue::single_quote(role)

    rlang::warn(
      glue::glue("Column, {var}, does not have role, {role}."))

    return(x)
  }

  if (nrow(x) == 1) {
    x$role <- NA_character_
  }
  else {
    x <- x[x$role != role,]
  }

  x
}

single_chr <- function(x, prefix = "", null_ok = FALSE) {
  arg <- paste0("`", prefix, "role", "`")

  if (null_ok && is.null(x)) {
    return(invisible(NULL))
  }

  if (length(x) != 1L) {
    rlang::abort(paste0(arg, " must have length 1."))
  }

  if (!is.character(x)) {
    rlang::abort(paste0(arg, " must be a character vector."))
  }

  if (is.na(x)) {
    rlang::abort(paste0(arg, " must not be `NA`."))
  }

  invisible(NULL)
}
