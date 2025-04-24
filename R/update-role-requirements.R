#' Update role specific requirements
#'
#' @description `update_role_requirements()` allows you to fine tune
#' requirements of the various roles you might come across in recipes (see
#' [update_role()] for general information about roles). Role requirements can
#' only be altered for roles that exist in the _original_ data supplied to
#' [recipe()], they are not applied to columns computed by steps.
#'
#' Like `update_role()`, `update_role_requirements()` is applied to the recipe
#' _immediately_, unlike the `step_*()` functions which do most of their work at
#' [prep()] time.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param recipe A recipe.
#'
#' @param role A string representing the role that you'd like to modify the
#'   requirements of. This must be a role that already exists in the recipe.
#'
#' @param bake At `bake()` time, should a check be done to ensure that all
#'   columns of this role that were supplied to `recipe()` also be present in
#'   the `new_data` supplied to `bake()`?
#'
#'   Must be a single `TRUE` or `FALSE`. The default, `NULL`, won't modify this
#'   requirement.
#'
#'   The following represents the default bake time requirements of specific
#'   types of roles:
#'
#'   - `"outcome"`: Not required at bake time. Can't be changed.
#'
#'   - `"predictor"`: Required at bake time. Can't be changed.
#'
#'   - `"case_weights"`: Not required at bake time by default.
#'
#'   - `NA`: Required at bake time by default.
#'
#'   - Custom roles: Required at bake time by default.
#'
#' @export
#' @examples
#' df <- tibble(y = c(1, 2, 3), x = c(4, 5, 6), var = c("a", "b", "c"))
#'
#' # Let's assume that you have a `var` column that isn't used in the recipe.
#' # We typically recommend that you remove this column before passing the
#' # `data` to `recipe()`, but for now let's pass it through and assign it an
#' # `"id"` role.
#' rec <- recipe(y ~ ., df) |>
#'   update_role(var, new_role = "id") |>
#'   step_center(x)
#'
#' prepped <- prep(rec, df)
#'
#' # Now assume you have some "new data" and you are ready to `bake()` it
#' # to prepare it for prediction purposes. Here, you might not have `var`
#' # available as a column because it isn't important to your model.
#' new_data <- df[c("y", "x")]
#'
#' # By default `var` is required at `bake()` time because we don't know if
#' # you actually use it in the recipe or not
#' try(bake(prepped, new_data))
#'
#' # You can turn off this check by using `update_role_requirements()` and
#' # setting `bake = FALSE` for the `"id"` role. We recommend doing this on
#' # the original unprepped recipe, but it will also work on a prepped recipe.
#' rec <- update_role_requirements(rec, "id", bake = FALSE)
#' prepped <- prep(rec, df)
#'
#' # Now you can `bake()` on `new_data` even though `var` is missing
#' bake(prepped, new_data)
update_role_requirements <- function(recipe, role, ..., bake = NULL) {
  check_dots_empty0(...)

  role <- vctrs::vec_cast(role, to = character())
  vctrs::vec_assert(role, size = 1L, arg = "role")
  role <- chr_explicit_na(role)

  # Role requirements can only be changed on the original data supplied to `recipe()`
  var_info <- recipe$var_info
  roles <- var_info$role
  roles <- chr_explicit_na(roles)

  exists <- role %in% roles
  if (!exists) {
    cli::cli_abort(
      c(
        x = "{.arg role} must be a preexisting role in the recipe.",
        i = "{.val {role}} is not a preexisting role."
      )
    )
  }

  recipe <- update_bake_role_requirements(recipe, role, bake)

  recipe
}

new_role_requirements <- function() {
  list(
    bake = new_bake_role_requirements()
  )
}

check_role_requirements <- function(
  recipe,
  new_data,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  check_bake_role_requirements(recipe, new_data, call = call)
  invisible(recipe)
}

get_role_requirements <- function(recipe) {
  recipe$requirements %||% new_role_requirements()
}

set_role_requirements <- function(recipe, requirements) {
  recipe$requirements <- requirements
  recipe
}

# ------------------------------------------------------------------------------
# `bake`

update_bake_role_requirements <- function(
  recipe,
  role,
  bake,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  if (is.null(bake)) {
    # Nothing to update
    return(recipe)
  }

  check_bool(bake, call = call)

  if (identical(role, "predictor")) {
    cli::cli_abort(
      c(
        x = "Can't update the {.arg bake} requirement of the \\
            {.val predictor} role.",
        i = "The {.val predictor} role is always required at {.fn bake} time."
      ),
      call = call
    )
  }
  if (identical(role, "outcome")) {
    cli::cli_abort(
      c(
        x = "Can't update the {.arg bake} requirement of the \\
            {.val outcome} role.",
        i = "The {.val outcome} role is never required at {.fn bake} time."
      ),
      call = call
    )
  }

  bakes <- get_bake_role_requirements(recipe)

  bakes[role] <- bake

  recipe <- set_bake_role_requirements(recipe, bakes)

  recipe
}

new_bake_role_requirements <- function() {
  set_names(logical(), nms = character())
}

check_bake_role_requirements <- function(
  recipe,
  new_data,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  var_info <- recipe$var_info
  var_names <- var_info$variable
  var_roles <- var_info$role
  var_roles <- chr_explicit_na(var_roles)

  requirements <- compute_bake_role_requirements(recipe)

  # Filter down to the roles that are actually required
  requirements <- requirements[requirements]
  requirement_roles <- names(requirements)

  # Find columns that match those roles
  names <- colnames(new_data)
  requirement_names <- var_names[var_roles %in% requirement_roles]

  exists <- requirement_names %in% names

  if (any(!exists)) {
    names <- requirement_names[!exists]
    roles <- unique(var_roles[var_names %in% names])

    standard <- roles == "predictor"
    any_nonstandard <- any(!standard)

    msg <- c(
      x = "The following required columns are missing from \\
          {.arg new_data}: {.var {names}}.",
      i = "These columns have one of the following roles, \\
          which are required at {.fn bake} time: {.var {roles}}."
    )

    if (any_nonstandard) {
      msg <- c(
        msg,
        i = "If these roles are not required at {.fn bake} time, use \\
        {.code update_role_requirements(role = \"your_role\", bake = FALSE)}."
      )
    }

    cli::cli_abort(msg, call = call)
  }

  invisible(recipe)
}

compute_bake_role_requirements <- function(recipe) {
  var_info <- recipe$var_info
  var_roles <- var_info$role
  var_roles <- chr_explicit_na(var_roles)
  var_roles <- unique(var_roles)

  # Start with default requirements
  requirements <- default_bake_role_requirements()

  # Drop unused default requirements
  requirements <- requirements[names(requirements) %in% var_roles]

  # Update with nonstandard roles in the recipe, which are required by default
  nonstandard_roles <- var_roles[!var_roles %in% names(requirements)]
  requirements[nonstandard_roles] <- TRUE

  # Override with `update_role_requirements()` changes
  user_requirements <- get_bake_role_requirements(recipe)
  requirements[names(user_requirements)] <- user_requirements

  requirements
}

get_bake_role_requirements <- function(recipe) {
  requirements <- get_role_requirements(recipe)
  requirements$bake
}

set_bake_role_requirements <- function(recipe, bake) {
  requirements <- get_role_requirements(recipe)
  requirements$bake <- bake
  set_role_requirements(recipe, requirements)
}

default_bake_role_requirements <- function() {
  # outcome:
  #   Forced to never be required. We never want the outcome to prevent a
  #   recipe from being baked.
  # predictor:
  #   Forced to always be required. You must have all predictors.
  # case_weights:
  #   Defaults to not required. Required if a step needs weights at bake time.
  #   In that case, a user may update this one to `TRUE`.
  # NA:
  #   Defaults to required. Assume that all unspecified roles are required to
  #   bake the recipe.
  # <other>:
  #   Fall through default for other roles (like "id" or "date") is `TRUE`.
  #   We assume they are required unless told otherwise.
  c(
    "outcome" = FALSE,
    "predictor" = TRUE,
    "case_weights" = FALSE,
    "NA" = TRUE
  )
}

# ------------------------------------------------------------------------------
# Helpers

chr_explicit_na <- function(x) {
  # To turn `NA_character_` into `"NA"` because you can't match
  # against `NA_character_` when assigning with `[<-`
  x[is.na(x)] <- "NA"
  x
}
