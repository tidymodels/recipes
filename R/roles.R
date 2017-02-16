## Adds a variable to the recipe$var_info table with a specific 
## role 
role_addition_char <- function(rec, vars, role) {
  if(length(role) > 1) stop("A single role is required")
  vars <- unique(vars)
  ## check if there are newly defined variables in the list 
  existing_var <- vars %in% rec$var_info$variable
  if(any(!existing_var)) {
    ## Add new variable with role
    new_vars <- tibble(variable = vars[!existing_var], role = rep(role, sum(!existing_var)))
    rec$var_info <- rbind(rec$var_info, new_vars)
  } else {
    ##   check for current roles != ""
    vars2 <- vars[existing_var]
    has_role <- rec$var_info$role[rec$var_info$variable %in% vars2] != ""
    if(any(has_role)) {
      warning(paste("Overwriting role(s) for",
                    paste(vars2[has_role], collapse = ", ", sep = "")))
    }
    rec$var_info$role[rec$var_info$variable %in% vars2] <- role
  }
  rec
}

role_addition_form <- function(rec, vars, role) {
  check_elements(vars, allowed = NULL)
  if(any(all.vars(vars) == "."))
    stop("Dots cannot be used here; use explicit variable names in the formula") 
  role_addition_char(rec = rec, all.vars(vars), role = role)
}

#' Manually Add Roles
#' 
#' \code{add_role} can add a role definition to an existing variable in the recipe.  
#' 
#' @param rec An existing \code{\link{recipe}}. 
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
add_role <- function(rec, vars, role = "predictor", data = NULL) {
  if(is_formula(vars) && !is.null(data)) 
    vars <- get_rhs_vars(vars, data)
  
  if(is.character(vars)) {
    out <- role_addition_char(rec = rec, vars = vars, role = role)
  } else if(is_formula(vars)) {
    out <- role_addition_form(rec = rec, vars = vars, role = role)
  } else stop("`vars` should be a character vector or formula")
  out$term_info <- out$role_info
  out
}
