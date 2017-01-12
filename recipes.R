
## `recipes.default` takes arguments:
##
##  - data: a template data set
##  - vars: a character strings of column names that 
##          will be used in the model is some capacity
##  - roles: an optional character string that shows 
##           the role of each element of `vars`

## `recipes.formula` takes arguments:
##
##  - data: a template data set
##  - formula: a formula (1- or 2-sided) that is used 
##             to declare the predictors (and possibly)
##             the outcomes
##  - should be added: subset?


recipe <- function(x, ...) UseMethod("recipe")

recipe.default <- function(data, vars = names(data), roles = NULL, ...) {
  require(tibble)
  require(dplyr)
  
  if(!is_tibble(data)) data <- as_tibble(data)
  if(is.null(vars)) vars <- colnames(data)
  if(any(table(vars) > 1))
    stop("`vars` should have unique members")
  if(any(!(vars %in% colnames(data))))
    stop("1+ elements of `vars` are not in `data`")
  
  data <- data[, vars]
  
  var_info <- tibble(variable = vars)
  
  ## Check and add roles when available
  if(!is.null(roles)) {
    if(length(roles) != length(vars))
      stop("The number of roles should be the same as the number of variables")
    var_info$role <- roles
  } else var_info$role <- ""
  
  ## Add types
  var_info <- get_types(data) %>% full_join(var_info)
  
  ## Return final object of class `recipe`
  out <- list(var_info = var_info, 
              steps = NULL,
              template = data)
  class(out) <- "recipe"
  out
}

recipe.formula <- function(formula, data, ...) {
  if(!is_formula(formula)) 
    formula <- as.formula(formula)
  
  require(lazyeval)
  require(tibble)
  
  if(!is_tibble(data)) data <- as_tibble(data)
  
  ## use lazyeval to get both sides of the formula
  outcomes <- get_lhs_vars(formula, data)
  predictors <- get_rhs_vars(formula, data)
  
  ## get `vars` from lhs and rhs of formula
  
  vars <- c(predictors, outcomes)
  
  ## subset data columns
  data <- data[, vars]
  
  ## derive roles
  roles <- rep("predictor", length(predictors))
  if(length(outcomes) > 0) 
    roles <- c(roles, rep("outcome", length(outcomes)))
  
  ## pass to recipe.default with vars and roles
  
  recipe.default(data = data, vars = vars, roles = roles, ...) 
}

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

###################################################################
## helpers

## Buckets variables into discrete, mutally exclusive types
get_types <- function(x) {
  type_list <- list(
    nominal = c("character", "factor", "ordered"),
    numeric = c("integer", "numeric"),
    date = c("POSIXct", "Date")
  )
  res <- lapply(type_list, type_by_var, dat = x)
  ## I think that Hadley has a better function somewhere to do this
  res <- res[sapply(res, length) > 0]
  for(i in seq_along(res)) 
    res[[i]] <- tibble(variable = res[[i]], type = names(res)[i])
  do.call("rbind", res)
}

type_by_var <- function(classes, dat) {
  res <- sapply(dat, is_one_of, what = classes)
  names(res)[res]
}

is_one_of <- function(x, what) {
  res <- sapply(
    as.list(what), 
    function(class, obj) inherits(obj, what = class),
    obj = x
  )
  any(res)
}

## general error trapping functions

check_outcomes_same_type <- function(x) x

## get variables from formulas
is_formula <- function(x) 
  isTRUE(inherits(x, "formula"))

get_lhs_vars <- function(formula, data) {
  if(!is_formula(formula)) 
    formula <- as.formula(formula)
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  formula <- as.formula(paste("~", deparse(f_lhs(formula))))
  get_rhs_vars(formula, data)
}

get_rhs_vars <- function(formula, data) {
  if(!is_formula(formula)) 
    formula <- as.formula(formula)
  ## This will need a lot of work to account for cases with `.`
  ## or embedded functions like `Sepal.Length + poly(Sepal.Width)`. 
  ## or should it? what about Y ~ log(x)? 
  data_info <- attr(model.frame(formula, data), "terms")
  response_info <- attr(data_info, "response")
  predictor_names <- names(attr(data_info, "dataClasses"))
  if(length(response_info)>0 && all(response_info > 0))
    predictor_names <- predictor_names[-response_info]
  predictor_names
}

get_lhs_terms <- function(x) x
get_rhs_terms <- function(x) x

## ancillary step functions

add_step <- function(rec, object) {
  rec$steps[[length(rec$steps)+1]] <- object
  rec
}


var_by_role <- function(rec, role = "predictor", returnform = TRUE) {
  res <- rec$var_info$variable[rec$var_info$role == role]
  if(returnform) 
    res <- as.formula(
      paste("~", 
            paste(res, collapse = "+")
      )
    )
  res
}

## Overall wrapper to make new step_X objects
step <- function(subclass, ...) {
  structure(
    list(
      ...
    ),
    class = c(paste0(subclass, "_step"), "step")
  )
}

learn   <- function(x, ...) UseMethod("learn")
process <- function(x, ...) UseMethod("process")

term_num <- function(x, num = 3, ...) {
  p <- length(x$col_names)
  if(p > 1) {
    out <- if(p >= num) 
      cat(paste0(x$col_names, collapse = ", ")) else 
        cat(p, "terms")
  } else out <- cat(" 1 term")
  out
}

###################################################################
## Centering functions

# other words to replace "apply": process, engineer, 

##   recipes(mtcars) %>% 
##     step_center()


## User called function that adds a classed object to the 
## original recipe.Add code to default formula to null and
## pick off all numeric variables
step_center <- function(recipe, formula) {
  # this should prob use get_rhs_{terms}
  col_names <- get_rhs_vars(formula, recipe$template) 
  add_step(recipe, step_center_new(col_names))
}

## Initializes a new object
step_center_new <- function(col_names = NULL, means = NULL) {
  step(
    subclass = "center", 
    col_names = col_names,
    means = means
  )
}

learn.center_step <- function(x, data, ...) {
  means <- unlist(lapply(data[, x$col_names], mean, na.rm = TRUE))
  step_center_new(x$col_names, means)
}


## ?inact, process, other verb to avoid conflicts with base:::apply
process.center_step <- function(x, data, ...) {
  data[, x$col_names] <- sweep(as.matrix(data[, x$col_names]), 2, x$means, "-")
  as_tibble(data)
}

print.center_step <- function(x, num = 3, ...) {
  cat("Centering with ")
  cat(term_num(x, num))
  if(!is.null(x$means)) cat(" (processed)\n") else cat("\n")
  invisible(x)
}

###################################################################
## Scaling functions

step_scale <- function(recipe, formula) {
  # this should prob use get_rhs_{terms}
  col_names <- get_rhs_vars(formula, recipe$template) 
  add_step(recipe, step_scale_new(col_names))
}

step_scale_new <- function(col_names = NULL, sds = NULL) {
  step(
    subclass = "scale", 
    col_names = col_names,
    sds = sds
  )
}

learn.scale_step <- function(x, data, ...) {
  sds <- unlist(lapply(data[, x$col_names], sd, na.rm = TRUE))
  step_scale_new(x$col_names, sds)
}

process.scale_step <- function(x, data, ...) {
  data[, x$col_names] <- sweep(as.matrix(data[, x$col_names]), 2, x$sds, "-")
  as_tibble(data)
}

print.scale_step <- function(x, num = 3, ...) {
  cat("Scaling with ")
  cat(term_num(x, num))
  if(!is.null(x$sds)) cat(" (processed)\n") else cat("\n")
  invisible(x)
}

###################################################################
## Dummy variables

step_dummy <- function(recipe, formula) {
  ## add an option to exclude cols with a single level? 
  ## add option to drop levels into "other" when freq is low
  ## add option to encode missing as a level
  col_names <- get_rhs_vars(formula, recipe$template) 
  col_data <- filter(recipe$var_info, variable %in% col_names)
  wrong_type <- col_data$type != "nominal"
  if(any(wrong_type)) {
    col_names <- col_data$variable[!wrong_type]
    if(length(col_names) == 0)
      stop("At least one nominal column should be in the formula")
    warning(paste("Some columns cannot be made into dummy variables:",
                  paste0(col_names, collapse = ", ")))
  }
  add_step(recipe, step_dummy_new(col_names))
}


step_dummy_new <- function(col_names = NULL,  
                           formula = NULL,
                           contrast = options("contrasts"),
                           naming = function(var, lvl) paste(var, lvl, sep = "_"),
                           levels = NULL) {
  step(
    subclass = "dummy", 
    col_names = col_names,
    formula = formula,
    contrast = contrast,
    naming = naming,
    levels = levels
  )
}

learn.dummy_step <- function(x, data, ...) {
  ## I hate doing this but currently we are going to have 
  ## to save the terms object form the original (= training) 
  ## data
  levels <- vector(mode = "list", length = length(x$col_names))
  for(i in seq_along(x$col_names)) {
    form <- as.formula(paste0("~", x$col_names[i]))
    terms <- model.frame(
      form, 
      data = data, 
      xlev = x$levels[[i]]
    )
    levels[[i]] <- attr(terms, "terms")
  }
  
  step_dummy_new(
    col_names = x$col_names, 
    formula = x$formula,
    contrast = x$contrast,
    naming = x$naming,
    levels = levels
  )
}

process.dummy_step <- function(x, data, ...) {
  ## Maybe do this in C? 
  
  for(i in seq_along(x$col_names)) {
    form <- as.formula(paste0("~", x$col_names[i]))
    indicators <- model.matrix(
      object = x$levels[[i]], 
      data = data
      # contrasts.arg = x$contrast 
    )
    indicators <- indicators[, -1, drop = FALSE]
    ## use backticks for nonstandard factor levels here 
    used_lvl <- gsub(paste0("^", x$col_names[i]), "", colnames(indicators))
    colnames(indicators) <- x$naming(x$col_names[i], used_lvl)
    data <- cbind(data, as.data.frame(indicators))
    data[, x$col_names[i]] <- NULL
  }
  as_tibble(data)
}

print.dummy_step <- function(x, num = 3, ...) {
  cat("Dummy variables from ")
  cat(term_num(x, num))
  if(!is.null(x$means)) cat(" (processed)\n") else cat("\n")
  invisible(x)
}

###################################################################
## Near-zero variance filter

###################################################################
## Near-zero variable (nzv) filter

step_nzv <- function(recipe, formula) {
  col_names <- get_rhs_vars(formula, recipe$template) 
  add_step(recipe, step_nzv_new(col_names))
}

step_nzv_new <- function(col_names = NULL,  
                         formula = NULL,
                         options = list(freqCut = 95 / 5, uniqueCut = 10, names = TRUE),
                         removals = NULL) {
  step(
    subclass = "nzv", 
    col_names = col_names,
    options = options,
    removals = removals
  )
}

learn.nzv_step <- function(x, data, ...) {
  data <- data[, x$col_names]
  filter <- do.call("nearZeroVar", c(list(x = data), x$ions))
  step_nzv_new(
    col_names = x$col_names, 
    options = x$options,
    removals = filter
  )
}

process.nzv_step <- function(x, data, ...) {
  if(length(x$removals) > 0)
    data <- data[, !(colnames(data) %in% x$removals)]
  as_tibble(data)
}

print.nzv_step <- function(x, num = 3, ...) {
  cat("Near-zero variance filter on")
  cat(term_num(x, num))
  if(!is.null(x$means)) cat(" (processed)\n") else cat("\n")
  invisible(x)
}

###################################################################
## Processing and application functions

learn.recipe <- function(x, training = x$template, verbose = TRUE) {
  if(length(x$steps) == 0)
    stop("Add some steps")
  
  preds <- x$var_info$variable[x$var_info$role == "predictor"]
  training <- as_tibble(training[, preds, drop = FALSE])
  
  for(i in seq(along = x$steps)) {
    if(verbose) cat("step", i, "\n")
    
    # Compute anything needed for the pre-processing steps 
    # then apply it to the current training set
    
    x$steps[[i]] <- learn(x$steps[[i]], data = training)
    training <- process(x$steps[[i]], data = training)
  }
  x
}

process.recipe <- function(x, newdata = x$template) {
  preds <- x$var_info$variable[x$var_info$role == "predictor"]
  training <- as_tibble(newdata[, preds, drop = FALSE])
  
  for(i in seq(along = x$steps)) {
    training <- process(x$steps[[i]], data = training)
  }
  training
}

print.recipe <- function(x, num = 3) {
  tab <- as.data.frame(table(x$var_info$role))
  colnames(tab) <- c("role", "#variables")
  cat("Data Recipe\n\n")
  cat("Inputs:\n\n")
  print(tab, row.names = FALSE)
  
  cat("\nSteps:\n\n")
  if(!is.null(x$steps)) {
    for(i in seq_along(x$steps)) 
      print(x$steps[[i]], num = num)
  }
  invisible(x)
}

