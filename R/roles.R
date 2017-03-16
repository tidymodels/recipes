#' Manually Add Roles
#'
#' \code{add_role} can add a role definition to an existing variable in the recipe.
#'
#' @param recipe An existing \code{\link{recipe}}.
#' @param vars A character string of variable names or simple formula that defines one or more variable that will be assigned to the role.
#' @param role A character string for a single role.
#' @param data Am optional data template that may be needed to resolve the formula (in case there is a \code{.}).
#' @return An updated recipe object.
#' @details If \code{vars} is a formula, it should not include any functions, negative signs, or \code{.} (since there is no data element to evaluate it).
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
#' @examples
#'
#' data(biomass)
#'
#' # Create the recipe manually
#' rec <- recipe(x = biomass)
#' rec
#' rec$var_info
#'
#' rec <- add_role(rec, vars = colnames(biomass)[3:7], role = "predictor")
#' rec <- add_role(rec, vars = "sample", role = "id variable")
#' rec <- add_role(rec, vars = "dataset", role = "splitting variable")
#' rec <- add_role(rec, vars = "HHV", role = "outcome")
#' rec
#'
#' # or, if you really want the formula method, you can change roles
#'
#' rec2 <- recipe(HHV ~ ., data = biomass)
#' # Now change inappropriate predictor roles
#' rec2 <- add_role(rec2, vars = "sample", role = "id variable")
#' rec2 <- add_role(rec2, vars = "dataset", role = "splitting variable")
#' rec2$var_info
#'
add_role <- function(recipe, vars, role = "predictor", data = NULL) {
  UseMethod("add_role", vars)
}

add_role.default <- function(recipe, vars, role = "predictor", data = NULL) {
  stop("`vars` should be either a character string or formula.", call. = FALSE)
}

#' @export
add_role.character <- function(recipe, vars, role = "predictor", data = NULL) {
  if(length(role) > 1) stop("A single role is required", call. = FALSE)
  vars <- unique(vars)
  ## check if there are newly defined variables in the list
  existing_var <- vars %in% recipe$var_info$variable
  if(any(!existing_var)) {
    ## Add new variable with role
    new_vars <- tibble(variable = vars[!existing_var], role = rep(role, sum(!existing_var)))
    recipe$var_info <- rbind(recipe$var_info, new_vars)
  } else {
    ##   check for current roles that are missing
    vars2 <- vars[existing_var]
    has_role <- !is.na(recipe$var_info$role[recipe$var_info$variable %in% vars2])
    if(any(has_role)) {
      warning("Overwriting role(s) for",
              paste0(vars2[has_role], collapse = ", "),
              call. = FALSE)
    }
    recipe$var_info$role[recipe$var_info$variable %in% vars2] <- role
  }
  recipe$term_info <- recipe$var_info
  recipe
}

#' @export
add_role.formula <- function(recipe, vars, role, data = NULL) {
  check_elements(vars, allowed = NULL)
  if(is_formula(vars) && !is.null(data)) {
    vars <- get_rhs_vars(vars, data)
  } else {
    if(any(all.vars(vars) == "."))
      stop("Dots cannot be used here; use explicit variable names in the formula or use `data`.",
           call. = FALSE)
    vars <- all.vars(vars)
  }

  add_role.character(recipe = recipe, vars = vars, role = role)
}
