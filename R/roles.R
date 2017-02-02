## Adds a variable to the recipe$var_info table with a specific 
## role 
role_addition_char <- function(rec, vars, role) {
  if(length(role) > 1) stop("A single role is required")
  vars <- unique(vars)
  already <- vars %in% rec$var_info$variable
  new_vars <- tibble(variable = vars[!already], role = rep(role, sum(!already)))
  rec$var_info <- rbind(rec$var_info, new_vars)
  if(any(already)) 
    warning(paste("A role already exists for",
                  paste(vars[already], collapse = ", ", sep = "")))
  rec
}

role_addition_form <- function(rec, vars, role) 
  role_addition_char(rec = rec, all.vars(vars), role = role)

add_role <- function(rec, vars, role = "predictor", data = NULL) {
  if(!is.null(data) & is_formula(vars)) 
    vars <- expand_dots(vars, data = data)
  
  if(is.character(vars)) {
    out <- role_addition_char(rec = rec, vars = vars, role = role)
  } else if(is_formula(vars)) {
    out <- role_addition_form(rec = rec, vars = vars, role = role)
  } else stop("`vars` should be a character vector or formula")
  out
}
