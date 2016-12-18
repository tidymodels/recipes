###################################################################
## Some simple prototyping files for data recipes. 

###################################################################
## Role functions

## Adds a variable to the recipe$var_info table with a specific 
## role 
role_addition <- function(rec, vars, role) {
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

add_predictor <- function(rec, vars, data = NULL) {
  role_addition(rec = rec, vars = vars, role = "predictor")
}

add_response <- function(rec, vars, data = NULL) {
  role_addition(rec = rec, vars = vars, role = "response")
}

add_weights <- function(rec, vars, data = NULL) {
  if(length(vars) > 1)
    stop("Multile variables cannot be used for case weights")
  role_addition(rec = rec, vars = vars, role = "case weight")
}

###################################################################
## Action functions. Right now, they produce objects with elements
## - action: a label
## - inputs: a character string of variable names (eventually + tags)
## - info: optional information about the operation
## - values: statistics to be added later (e.g. rotation, etc) from data

standardize <- function(rec, features, type = c("center", "scale")) {
  if(length(features) == 0) {
    warning("No standardization added to recipe")
  } else {
    if("center" %in% type) 
      rec$actions[[length(rec$actions)+1]] <- list(action = "center", inputs = features)
    if("scale" %in% type) 
      rec$actions[[length(rec$actions)+1]]  <- list(action = "scale", inputs = features)
  }
  rec
}

get_int_data <- function(x) {
  out <- list(form = as.formula(paste0("~", x)))
  out$variables <- all.vars(out$form) 
  out
}

get_all_ints <- function(x) {
  if(!is.list(x)) {
    nms <- x
    x <- as.list(x)
    names(x) <- nms
  }
  lapply(x, get_int_data)
}

interact <- function(rec, form, data = NULL) {
  features <- if(is.character(form)) 
    form else 
      attr(terms.formula(form), "term.labels")
  features <- features[grepl(":", features)]
  info <- get_all_ints(features)
  rec$actions[[length(rec$actions)+1]] <- 
    list(action = "interaction", 
         inputs = unique(unlist(lapply(info, function(x) x$variables))), 
         info = info)
  rec
}

pca_extract <- function(rec, features, num = NA) {
  if(length(features) == 0) {
    warning("No PCA extraction added to recipe")
  } else {
    rec$actions[[length(rec$actions)+1]] <- 
      list(action = "pca signal extraction", inputs = features, num = num)
  }
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
    var_info <- tibble(variable = colnames(data), role = "")
  }
  out <- list(var_info = var_info, actions = NULL)
  class(out) <- "recipe"
  out
}

print.recipe <- function(x, limit = 5, ...){
  if(!is.null(x$var_info) && nrow(x$var_info) > 0) {
    tab <- as.data.frame(table(x$var_info$role))
    colnames(tab) <- c("role", "#variables")
    cat("Data Recipe\n\n")
    cat("Ingredients:\n\n")
    print(tab, row.names = FALSE)
  } else cat("Data Recipe (empty)\n")
  
  if(length(x$actions) > 0) {
    
    act_in <- lapply(x$actions, function(x) x$inputs)
    act_nm <- unlist(lapply(x$actions, function(x) x$action))
    if(any(unlist(lapply(act_in, length)) > limit)) {
      act_in <- unlist(lapply(act_in, length))
      act_in <- data.frame(action = act_nm,
                           inputs = act_in)
      colnames(act_in)[2] <- "#inputs"
      cat("\nActions (in order):\n\n")
      print(act_in, row.names = FALSE)
    } else {
      act_in <- unlist(lapply(act_in, paste0, collapse = ", "))
      act_in <- paste0(" ", act_nm, ": ", act_in)
      cat("\nActions and Inputs (in order):\n\n")
      cat(act_in, sep = "\n")
    }
  } else  cat("\nNo Actions\n")
  invisible(x)
}



