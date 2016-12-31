###################################################################
## Some simple prototyping files for data recipes. 

## Add missing data arguments
## What is the procedure for getting different components
## (i.e. predictors, outcomes)? 

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
    dplyr::filter(variable %in% vars)
  wrong_role <- role_data$role != role
  if(any(wrong_role))
    stop(paste("These variables do not have the correct role",
               "for this operation:", 
               paste(role_data$variable[wrong_role], collapse = ", ")))
  
  ## Third, if the data types are available, then 
  ## check. Avoid this check with types are missing. This
  ## makes sense for interactions and other situations where
  ## any type will do
  if(!all(is.na(types))) {
    role_data <- role_data %>% dplyr::filter(type != "")
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

compute_center <- function(form, data, args) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  ## This can prob be done using some tibble thingy
  dat <- model.matrix(form, data = data)[, -1, drop  = FALSE]
  means <- apply(dat, 2, mean, na.rm = TRUE) 
  list(means = means, columns = colnames(dat))
}

apply_center <- function(form, data, values) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  dat <- model.matrix(form, data = data)[, -1, drop  = FALSE]
  dat <- sweep(dat, 2, values$means, "-")
  as_tibble(dat)
}

compute_scale <- function(form, data, args) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  dat <- model.matrix(form, data = data)[, -1, drop  = FALSE]
  sds <- apply(dat, 2, sd, na.rm = TRUE) 
  list(sds = sds, columns = colnames(dat))
}

apply_scale <- function(form, data, values) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  dat <- model.matrix(form, data = data)[, -1, drop  = FALSE]
  dat <- sweep(dat, 2, values$sds, "/")
  as_tibble(dat)
}

standardize <- function(rec, form, type = c("center", "scale"), data = NULL) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  
  check_vars(form, role = "predictor", rec = rec, types = "numeric")
  
  if("center" %in% type) 
    rec$verbs[[length(rec$verbs)+1]] <- 
    list(verb = "center", inputs = form, 
         compute = compute_center, apply = apply_center, 
         args = NULL)
  if("scale" %in% type) 
    rec$verbs[[length(rec$verbs)+1]]  <- 
    list(verb = "scale", inputs = form, 
         compute = compute_scale, apply = apply_scale, 
         args = NULL)
  rec
}

interact <- function(rec, form, data = NULL) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  check_vars(form, role = "predictor", rec = rec, types = NA)
  
  rec$verbs[[length(rec$verbs)+1]] <- 
    list(verb = "interaction", inputs = form, 
         args = NULL)
  rec
}

pca_extract <- function(rec, form, data = NULL, num = NULL, 
                        prcomp_args = list(retx = FALSE)) {
  if(!is_formula(form))
    stop("`form` should be a formula with predictors listed on the right-hand side")
  if(!is.null(data)) 
    form <- expand_dots(form, data = data)
  
  check_vars(form, role = "predictor", rec = rec, types = "numeric")
  
  rec$verbs[[length(rec$verbs)+1]] <- 
    list(verb = "pca signal extraction", inputs = form, 
         compute = compute_pca_extract, apply = apply_pca_extract, 
         args = list(num = num, standardize = standardize, prcomp = prcomp_args))
  rec
}


compute_pca_extract <- function(form, data, args) {
  ## add zv filter? 
  
  ## use this call as the basis to make one for `prcomp`
  cl <- match.call(expand.dots = TRUE)
  cl$args <- NULL
  cl[[1]] <- quote(stats:::prcomp.formula)
  names(cl)[2] <- "formula"
  ## surely a smoother way exists
  for(i in seq(along = args$prcomp)) 
    cl[[ names(args$prcomp)[i] ]] <- args$prcomp[[i]]
  comp_info <- eval(cl, envir = parent.frame())
  # Better api here
  if(is.null(args$num))
    args$num <- max(which(cumsum(comp_info$sdev^2/sum(comp_info$sdev^2)) <= .95))
  
  ## keep everything past the requested rotation? 
  ## comp_info$rotation <- comp_info$rotation[,1:args$num, drop = FALSE]
  list(components = comp_info, num = args$num, columns = paste0("PC", seq_lab(1:args$num)))
}

apply_pca_extract <- function(form, data, values) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  dat <- model.matrix(form, data = data)[, -1, drop  = FALSE]
  dat <- predict(values$components, dat)[, 1:values$num]
  colnames(dat) <- values$columns
  as_tibble(dat)
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
  } ## need to add the else here
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
    ## need to catch multivariate outcomes and capture vars accordingly
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

## do we really need the `data` argument here? 
get_columns <- function(form, data) {
  if(!is_formula(form)) 
    form <- as.formula(form)
  attr(terms(form, data = schedulingData), "term.labels")
}

seq_lab <- function(x) 
  gsub(" ", "0", format(seq_along(x)))


###################################################################
## Functions for training and application of the pre-processing.
## Better names/API are needed

## should we always return a tibble?
## TODO At each iteration check to see if values are there and then skip
## in case of adding to a recipe that has already been computed. This happens
## since the recipe contains the values. Should we make a different 
## object? 
## TODO Option to remove variables that PCs are replacing? What does this do to 
## subsequent formulas? 

compute_predictors <- function(rec, training, verbose = TRUE, keep_data = FALSE) {
  preds <- rec$var_info$variable[rec$var_info$role == "predictor"]
  training <- as_tibble(training[, preds, drop = FALSE])
  vrb <- rec$verbs
  for(i in seq(along = vrb)) {
    if(verbose) cat(vrb[[i]]$verb, "\n")
    
    # Compute anything needed for the pre-processing steps including
    # the names of the columns that will be replaced (or new columns). 
    # We might need to split the `columns` return object into 
    # seperate sets to replace, add, or remove columns. 
    vrb[[i]]$values <- vrb[[i]]$compute(as.formula(vrb[[i]]$inputs), training, vrb[[i]]$values$args)
    cols <- vrb[[i]]$values$columns
    # Apply the results to the original data
    training[, cols] <- vrb[[i]]$apply(as.formula(vrb[[i]]$inputs), training, vrb[[i]]$values)
  }
  rec$verbs <- vrb
  if(keep_data) 
    rec$data <- training
  rec
}

apply_predictors <- function(rec, data, verbose = TRUE) {
  preds <- rec$var_info$variable[rec$var_info$role == "predictor"]
  data <- data[, preds, drop = FALSE]
  vrb <- rec$verbs
  for(i in seq(along = vrb)) {
    if(verbose) cat(vrb[[i]]$verb, "\n")
    cols <- vrb[[i]]$values$columns
    data[, cols] <- vrb[[i]]$apply(vrb[[i]]$inputs, data, vrb[[i]]$values)
  }
  data
}
