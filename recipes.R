###################################################################
## Some simple prototyping files for data recipes. 

###################################################################
## Role functions

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

## huh, I assumed this was in R. Crappy version:
is_formula <- function(x) {
  isTRUE(inherits(x, "formula"))
}

## Expand a rhs dot if used and data is passed in 
expand_dots <- function(form, data) {
  form <- terms(form, data = data)
  attributes(form) <- NULL
  form
}

missing_msg <- function(x) {
  msg <- if(length(x) > 1)
    "These variables are" else
      "This variable is"
  paste(msg, "not currently included in the recipe:", 
        paste0(x, collapse = ", "))
}

check_vars <- function(vars, role, types, rec){
  ## First check to see if they are listed in the list of
  ## raw variables. If not this may not matter in case
  ## they are derived variables. For now, issue a 
  ## warning if they are not found.
  ## NOTE: all.vars does not remove "-vars" terms.
  vars <- all.vars(vars)
  missing_vars <- vars[!(vars %in% rec$var_info$variable)]
  if(length(missing_vars) > 0)
    warning(missing_msg(missing_vars))
  
  ## Second, see if they are the right role
  role_data <- rec$var_info %>% 
    filter(variable %in% vars)
  wrong_role <- role_data$role != role
  if(any(wrong_role))
    stop(paste("These variables do not have the correct role",
               "for this operation:", 
               paste(role_data$variable[wrong_role], collapse = ", ")))
  
  ## Third, if the data types are availible, then 
  ## check. Avoid this check wuth types are missing. This
  ## makes sense for interactions and other situations where
  ## any type will do
  if(!all(is.na(types))) {
    role_data <- role_data %>% filter(type != "")
    wrong_type <- !(role_data$type %in% types)
    if(any(wrong_type))
      stop(paste("These variables do not have the correct role",
                 "for this operation:", 
                 paste(role_data$variable[wrong_type], collapse = ", ")))
  }
  invisible(NULL)
}

## TODO test cases terms(~ . - Species, data = iris)

###################################################################
## Verb specification functions. 

standardize <- function(rec, form, type = c("center", "scale"), data = NULL) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  
  check_vars(form, role = "predictor", rec = rec, types = "numeric")
  
  if("center" %in% type) 
    rec$verbs[[length(rec$verbs)+1]] <- list(verb = "center", inputs = form)
  if("scale" %in% type) 
    rec$verbs[[length(rec$verbs)+1]]  <- list(verb = "scale", inputs = form)
  rec
}

interact <- function(rec, form, data = NULL) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  check_vars(form, role = "predictor", rec = rec, types = NA)
  
  rec$verbs[[length(rec$verbs)+1]] <- 
    list(verb = "interaction", inputs = form)
  rec
}

pca_extract <- function(rec, form, data = NULL, num = NA, standardize = FALSE) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  
  check_vars(form, role = "predictor", rec = rec, types = "numeric")
  
  rec$verbs[[length(rec$verbs)+1]] <- 
    list(verb = "pca signal extraction", inputs = form, 
         num = num, standardize = standardize)
  rec
}


###################################################################
## Other functions

## initialize a new recipe. eventually add code to pull variable
## names and types if `data` is supplied, 
recipe <- function(data = NULL, vars = NULL, roles = NULL) {
  var_info <- NULL
  if(!is.null(data) && is.null(vars) && is.null(roles)) {
    if(is.matrix(data)) data <- as_tibble(data)
    var_info <- tibble(variable = colnames(data), role = "", type = "")
  }
  out <- list(var_info = var_info, verbs = NULL)
  class(out) <- "recipe"
  out
}

print.recipe <- function(x, limit = 3, ...){
  if(!is.null(x$var_info) && nrow(x$var_info) > 0) {
    tab <- as.data.frame(table(x$var_info$role))
    colnames(tab) <- c("role", "#variables")
    cat("Data Recipe\n\n")
    cat("Inputs:\n\n")
    print(tab, row.names = FALSE)
  } else cat("Data Recipe (empty)\n")
  
  if(length(x$verbs) > 0) {
    cat("\nVerbs and Inputs (in order):\n\n")
    vrb_sum <- lapply(x$verbs, 
                      function(x, lim) 
                        paste0(" ", x$verb, ": ", print_verbs(x$input, max_vars = lim), "\n"), 
                      lim = limit)
    cat(paste0(vrb_sum, collapse = ""))
  } else  cat("\nNo verbs\n")
  invisible(x)
}

## Doesn't work properly with interactions =[
## Someday write some general code to cut the text based
## on the page width
print_verbs <- function(x, max_vars = 5) {
  x <- terms(as.formula(x))
  vars <- attr(x, "term.labels")
  p <- length(vars)
  if(p == 0) return("(none)")
  print_vars <- paste0(vars[1:min(p, max_vars)], collapse = ", ")
  if(p > max_vars) 
    print_vars <- paste0(print_vars, " (", p - max_vars, " others)")
  print_vars
}



as.recipe <- function(formula, data = NULL) {
  ## currently ignores interactions in the formula but
  ## can be added later in the function via a verb 
  if(!is.null(data)) {
    data_info <- attr(model.frame(formula, data), "terms")
    response_info <- attr(data_info, "response")
    data_classes <- attr(data_info, "dataClasses")
    var_info <- tibble(variable = names(data_classes),
                       role = "",
                       type = data_classes)
    if(length(response_info) > 0) {
      var_info[ response_info, "role"] <- "response"
      var_info[-response_info, "role"] <- "predictor"
    } else {
      var_info$role <- "predictor"
    }
  } else {
    ## need to catch mutivariate outcomes and capture vars accordingly
    data_info <- terms(formula)
    response_info <- attr(data_info, "response")
    var_info <- tibble(variable = rownames(attr(data_info, "factors")),
                       role = "",
                       type = "")
    if(length(response_info) > 0) {
      var_info[ response_info, "role"] <- "response"
      var_info[-response_info, "role"] <- "predictor"
    } else {
      var_info$role <- "predictor"
    }
  }
  
  out <- list(var_info = var_info, verbs = NULL)
  class(out) <- "recipe"
  out
}
