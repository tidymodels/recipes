
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

form_printer <- function(x, wdth = 50, ...) {
  if(!is_formula(x)) 
    x <- as.formula(x)
  char_x <- deparse(f_rhs(x))
  if(sum(nchar(char_x)) >= wdth) {
    split_up <- unlist(strsplit(char_x, split = " \\+ "))
    widths <- which(cumsum(nchar(split_up)) <= wdth)
    split_up <- if(length(widths) == length(split_up))
      split_up else c(split_up[widths], "{more}")  
    out <- paste0(split_up, collapse = " + ")
  } else out <- char_x
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
  add_step(recipe, step_center_new(formula))
}

## Initializes a new object
step_center_new <- function(formula = NULL, means = NULL) {
  step(
    subclass = "center", 
    formula = formula,
    means = means
  )
}

learn.center_step <- function(x, data, ...) {
  col_names <- get_rhs_vars(x$formula, data) 
  means <- unlist(lapply(data[, col_names], mean, na.rm = TRUE))
  step_center_new(x$formula, means)
}

## ?inact, process, other verb to avoid conflicts with base:::apply
process.center_step <- function(x, data, ...) {
  data[, names(x$means)] <- sweep(as.matrix(data[, names(x$means)]), 2, x$means, "-")
  as_tibble(data)
}

print.center_step <- function(x, form_width = 50, ...) {
  cat("Centering with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$means)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

###################################################################
## Scaling functions

step_scale <- function(recipe, formula) {
  add_step(recipe, step_scale_new(formula))
}

step_scale_new <- function(formula = NULL, sds = NULL) {
  step(
    subclass = "scale", 
    formula = formula,
    sds = sds
  )
}

learn.scale_step <- function(x, data, ...) {
  col_names <- get_rhs_vars(x$formula, data) 
  sds <- unlist(lapply(data[, col_names], sd, na.rm = TRUE))
  step_scale_new(formula = x$formula, sds)
}

process.scale_step <- function(x, data, ...) {
  data[, names(x$sds)] <- sweep(as.matrix(data[, names(x$sds)]), 2, x$sds, "-")
  as_tibble(data)
}

print.scale_step <- function(x, form_width = 50, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$sds)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

###################################################################
## Dummy variables

step_dummy <- function(recipe, formula) {
  add_step(recipe, step_dummy_new(formula))
}


step_dummy_new <- function(formula = NULL,  
                           contrast = options("contrasts"),
                           naming = function(var, lvl) paste(var, lvl, sep = "_"),
                           levels = NULL) {
  step(
    subclass = "dummy", 
    formula = formula,
    contrast = contrast,
    naming = naming,
    levels = levels
  )
}

learn.dummy_step <- function(x, data, ...) {
  col_names <- get_rhs_vars(x$formula, data) 
  ## I hate doing this but currently we are going to have 
  ## to save the terms object form the original (= training) 
  ## data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  for(i in seq_along(col_names)) {
    form <- as.formula(paste0("~", col_names[i]))
    terms <- model.frame(
      form, 
      data = data, 
      xlev = x$levels[[i]]
    )
    levels[[i]] <- attr(terms, "terms")
  }
  
  step_dummy_new(
    formula = x$formula,
    contrast = x$contrast,
    naming = x$naming,
    levels = levels
  )
}

process.dummy_step <- function(x, data, ...) {
  ## Maybe do this in C? 
  col_names <- names(x$levels)
  for(i in seq_along(x$levels)) {
    form <- as.formula(paste0("~", x$levels[i]))
    indicators <- model.matrix(
      object = x$levels[[i]], 
      data = data
      # contrasts.arg = x$contrast 
    )
    indicators <- indicators[, -1, drop = FALSE]
    ## use backticks for nonstandard factor levels here 
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- x$naming(col_names[i], used_lvl)
    data <- cbind(data, as.data.frame(indicators))
    data[, col_names[i]] <- NULL
  }
  as_tibble(data)
}

print.dummy_step <- function(x, form_width = 50, ...) {
  cat("Dummy variables from ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$levels)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

###################################################################
## Near-zero variable (nzv) filter

step_nzv <- function(recipe, formula) {
  add_step(recipe, step_nzv_new(formula))
}

step_nzv_new <- function(formula = NULL,
                         options = list(freqCut = 95 / 5, uniqueCut = 10, names = TRUE),
                         removals = NULL) {
  step(
    subclass = "nzv", 
    formula = formula,
    options = options,
    removals = removals
  )
}

learn.nzv_step <- function(x, data, ...) {
  col_names <- get_rhs_vars(x$formula, data) 
  data <- data[, col_names]
  filter <- do.call("nearZeroVar", c(list(x = data), x$options))
  step_nzv_new(
    formula = x$formula, 
    options = x$options,
    removals = filter
  )
}

process.nzv_step <- function(x, data, ...) {
  if(length(x$removals) > 0)
    data <- data[, !(colnames(data) %in% x$removals)]
  as_tibble(data)
}

print.nzv_step <- function(x, form_width = 50, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$means)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

###################################################################
## PCA extraction

step_pca_new <- function(formula = NULL,
                         num  = 5, 
                         options = list(center = TRUE, scale. = TRUE),
                         object = NULL) {
  
  step(
    subclass = "pca",
    formula = formula, 
    num = num,
    object = object
  )
}

step_pca <- function(recipe, formula) {
  add_step(recipe, step_pca_new(formula))
}

learn.pca_step <- function(x, data, ...) {
  col_names <- get_rhs_vars(x$formula, data) 
  dat <- data[, col_names]
  prc <- do.call("prcomp", c(list(x = dat), x$options))
  
  step_pca_new(
    formula = x$formula,
    num = min(x$num, ncol(dat)),
    options = x$options,
    object = prc
  )
}

process.pca_step <- function(x, data, ...) {
  pca_vars <- rownames(x$object$rotation)
  comps <- predict(x$object, data[, pca_vars, drop = FALSE])
  comps <- comps[, 1:x$num, drop = FALSE]
  data <- cbind(data, comps)
  data <- data[, !(colnames(data) %in% pca_vars), drop = FALSE]
  as_tibble(data)
}


print.pca_step <- function(x, form_width = 50, ...) {
  cat("PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$object)) cat(" [learned]\n") else cat("\n")
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

print.recipe <- function(x, form_width = 50) {
  tab <- as.data.frame(table(x$var_info$role))
  colnames(tab) <- c("role", "#variables")
  cat("Data Recipe\n\n")
  cat("Inputs:\n\n")
  print(tab, row.names = FALSE)
  
  cat("\nSteps:\n\n")
  if(!is.null(x$steps)) {
    for(i in seq_along(x$steps)) 
      print(x$steps[[i]], form_width = form_width)
  }
  invisible(x)
}

