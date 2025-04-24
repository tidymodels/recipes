#' Manually alter roles
#'
#' @description `update_role()` alters an existing role in the recipe or assigns
#' an initial role to variables that do not yet have a declared role.
#'
#' `add_role()` adds an _additional_ role to variables that already have a role
#' in the recipe. It does not overwrite old roles, as a single variable can have
#' multiple roles.
#'
#' `remove_role()` eliminates a single existing role in the recipe.
#' @param recipe An existing [recipe()].
#'
#' @param ... One or more selector functions to choose which variables are being
#'   assigned a role. See [selections()] for more details.
#' @param new_role A character string for a single role.
#' @param new_type A character string for specific type that the variable should
#'   be identified as. If left as `NULL`, the type is automatically identified
#'   as the _first_ type you see for that variable in `summary(recipe)`.
#' @param old_role A character string for the specific role to update for the
#'   variables selected by `...`. `update_role()` accepts a `NULL` as long as
#'   the variables have only a single role.
#'
#' @return An updated recipe object.
#'
#' @details
#'
#' `update_role()`, `add_role()` and `remove_role()` will be applied on a recipe
#' before any of the steps or checks, regardless of where they are located in
#' position. This means that roles can only be changed with these three
#' functions for columns that are already present in the original data supplied
#' to `recipe()`. See the `role` argument in some step functions to update roles
#' for columns created by steps.
#'
#' Variables can have any arbitrary role (see the examples) but there are three
#' special standard roles, `"predictor"`, `"outcome"`, and `"case_weights"`. The
#' first two roles are typically required when fitting a model.
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
#' don't fall in the standard `"predictor"` bucket. You can perform a step on
#' all of the variables that have a custom role with the selector [has_role()].
#'
#' ```{r, child = "man/rmd/non-standard-roles.Rmd"}
#' ```
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(biomass, package = "modeldata")
#'
#' # Using the formula method, roles are created for any outcomes and predictors:
#' recipe(HHV ~ ., data = biomass) |>
#'   summary()
#'
#' # However `sample` and `dataset` aren't predictors. Since they already have
#' # roles, `update_role()` can be used to make changes, to any arbitrary role:
#' recipe(HHV ~ ., data = biomass) |>
#'   update_role(sample, new_role = "id variable") |>
#'   update_role(dataset, new_role = "splitting variable") |>
#'   summary()
#'
#' # `update_role()` cannot set a role to NA, use `remove_role()` for that
#' \dontrun{
#' recipe(HHV ~ ., data = biomass) |>
#'   update_role(sample, new_role = NA_character_)
#' }
#'
#' # Variables can have more than one role. `add_role()` can be used
#' # if the column already has at least one role:
#' recipe(HHV ~ ., data = biomass) |>
#'   add_role(carbon, sulfur, new_role = "something") |>
#'   summary()
#'
#' # `update_role()` has an argument called `old_role` that is required to
#' # unambiguously update a role when the column currently has multiple roles.
#' recipe(HHV ~ ., data = biomass) |>
#'   add_role(carbon, new_role = "something") |>
#'   update_role(carbon, new_role = "something else", old_role = "something") |>
#'   summary()
#'
#' # `carbon` has two roles at the end, so the last `update_role()` fails since
#' # `old_role` was not given.
#' \dontrun{
#' recipe(HHV ~ ., data = biomass) |>
#'   add_role(carbon, sulfur, new_role = "something") |>
#'   update_role(carbon, new_role = "something else")
#' }
#'
#' # To remove a role, `remove_role()` can be used to remove a single role.
#' recipe(HHV ~ ., data = biomass) |>
#'   add_role(carbon, new_role = "something") |>
#'   remove_role(carbon, old_role = "something") |>
#'   summary()
#'
#' # To remove all roles, call `remove_role()` multiple times to reset to `NA`
#' recipe(HHV ~ ., data = biomass) |>
#'   add_role(carbon, new_role = "something") |>
#'   remove_role(carbon, old_role = "something") |>
#'   remove_role(carbon, old_role = "predictor") |>
#'   summary()
#'
#' # If the formula method is not used, all columns have a missing role:
#' recipe(biomass) |>
#'   summary()
#' @name roles
NULL

#' @export
#' @rdname roles
add_role <- function(recipe, ..., new_role = "predictor", new_type = NULL) {
  check_string(new_role, allow_empty = FALSE)
  check_string(new_type, allow_empty = FALSE, allow_null = TRUE)

  if (new_role == "case_weights") {
    cli::cli_abort(
      c(
        "!" = "Roles of {.val case_weights} cannot be set using \\
      {.fn add_role}.",
        "i" = "Please use {.help hardhat::frequency_weights} or \\
      {.help hardhat::importance_weights} to specify case weights \\
      before the data is passed to {.fn recipe}."
      )
    )
  }

  # Roles can only be changed on the original data supplied to `recipe()`,
  # so this is safe
  data <- recipe$template
  info <- recipe$var_info
  terms <- quos(...)

  vars <- recipes_eval_select(terms, data, info, check_case_weights = FALSE)

  if (length(vars) == 0L) {
    cli::cli_warn("No columns were selected in {.fn add_role}.")
    return(recipe)
  }

  case_weights_vars <- info |>
    filter(role == "case_weights", variable %in% vars)
  if (nrow(case_weights_vars) > 0) {
    cli::cli_abort(
      "{.fn add_role} cannot be used on variables with role \\
      {.val case_weights}."
    )
  }

  # Check to see if role already exists
  # remove variables where the role already exists
  existing_var_idx <- recipe$var_info$variable %in% vars

  if (all(is.na(recipe$var_info$role[existing_var_idx]))) {
    cli::cli_abort(
      c(
        "!" = "No role currently exists for column{?s}: {.var {vars}}.",
        "i" = "Please use {.fn update_role} instead."
      )
    )
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

    cli::cli_warn(
      "Role {.val {new_role}} already exists for column{?s}: \\
      {.and {.var {vars_that_role_exists_for}}}. Skipping."
    )

    vars <- vars[!(vars %in% vars_that_role_exists_for)]
  }

  # Pull in first type we come across if unspecified
  if (is.null(new_type)) {
    new_type <- purrr::map(
      vars,
      ~ {
        first_row_with_var <- which(recipe$var_info$variable == .x)[1]
        recipe$var_info$type[[first_row_with_var]]
      }
    )
  } else {
    new_type <- as.list(rep(new_type, times = length(vars)))
  }

  source <- purrr::map_chr(
    vars,
    ~ {
      first_row_with_var <- which(recipe$var_info$variable == .x)[1]
      recipe$var_info$source[first_row_with_var]
    }
  )

  for (var in vars) {
    old_roles <- recipe$var_info$role[recipe$var_info$variable == var]

    if (new_role == "predictor" && any(old_roles == "outcome")) {
      cli::cli_abort(
        "{.var {var}} cannot get {.val predictor} role as it already \\
          has role {.val outcome}."
      )
    }

    if (new_role == "outcome" && any(old_roles == "predictor")) {
      cli::cli_abort(
        "{.var {var}} cannot get {.val outcome} role as it already \\
        has role {.val predictor}."
      )
    }
  }

  for (i in seq_along(vars)) {
    last_row_with_var <- dplyr::last(which(recipe$var_info$variable == vars[i]))
    recipe$var_info <- tibble::add_row(
      .data = recipe$var_info,
      variable = unname(vars[i]),
      type = list(unname(new_type[[i]])),
      role = new_role,
      source = unname(source[i]),
      .after = last_row_with_var
    )
  }

  recipe$term_info <- recipe$var_info
  recipe
}

#' @export
#' @rdname roles
update_role <- function(recipe, ..., new_role = "predictor", old_role = NULL) {
  check_string(new_role, allow_empty = FALSE)
  check_string(old_role, allow_empty = FALSE, allow_null = TRUE)

  if (new_role == "case_weights") {
    cli::cli_abort(
      c(
        "!" = "Roles of {.val case_weights} cannot be set using \\
      {.fn update_role}.",
        "i" = "Please use {.help hardhat::frequency_weights} or \\
      {.help hardhat::importance_weights} to specify case weights \\
      before the data is passed to {.fn recipe}."
      )
    )
  }

  # Roles can only be changed on the original data supplied to `recipe()`,
  # so this is safe
  data <- recipe$template
  info <- recipe$var_info
  terms <- quos(...)

  vars <- recipes_eval_select(terms, data, info, check_case_weights = FALSE)

  if (length(vars) == 0L) {
    cli::cli_warn("No columns were selected in {.fn update_role}.")
    return(recipe)
  }

  case_weights_vars <- info |>
    filter(role == "case_weights", variable %in% vars)
  if (nrow(case_weights_vars) > 0) {
    cli::cli_abort(
      "{.fn update_role} cannot be used on variables with role \\
      {.val case_weights}."
    )
  }

  # check to see if any variables have multiple roles
  if (is.null(old_role)) {
    var_counts <-
      info |>
      dplyr::filter(variable %in% vars) |>
      dplyr::group_by(variable) |>
      dplyr::count()
    if (any(var_counts$n > 1)) {
      cli::cli_abort(
        "{.arg old_role} can only be {.code NULL} when the variable(s) have a \\
        single existing role."
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
  check_string(old_role, allow_empty = FALSE)

  if (old_role == "case_weights") {
    cli::cli_abort(
      "Roles of {.val case_weights} cannot removed using {.fn remove_role}."
    )
  }

  # Roles can only be changed on the original data supplied to `recipe()`,
  # so this is safe
  data <- recipe$template
  info <- recipe$var_info
  terms <- quos(...)

  vars <- recipes_eval_select(terms, data, info)

  if (length(vars) == 0L) {
    cli::cli_warn("No columns were selected in {.fn remove_role}.")
    return(recipe)
  }

  info <- info |>
    mutate(.orig_order = seq_len(nrow(info))) |>
    group_by(variable) |>
    do(role_rm_machine(., role = old_role, var = vars)) |>
    ungroup() |>
    arrange(.orig_order) |>
    dplyr::select(-.orig_order)

  recipe$var_info <- info
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
    cli::cli_warn(
      "Column {.var {x$variable[1]}} does not have role {.val {role}}."
    )

    return(x)
  }

  if (nrow(x) == 1) {
    x$role <- NA_character_
  } else {
    x <- x[x$role != role, ]
  }

  x
}
