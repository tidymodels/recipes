## User called function that adds a classed object to the 
## original recipe. 
step_center <- function(recipe, terms, role = NA, means = NULL) {
  add_step(recipe, step_center_new(terms = terms, role = role, means = means))
}

## Initializes a new object
step_center_new <- function(terms = NULL, role = NA, means = NULL) {
  step(
    subclass = "center", 
    terms = terms,
    role = role,
    means = means
  )
}

## The learn functions have the and centering info but 
## does not have access to the recipe so no var_info. 
## We might want to check against the roles and types. 
## That might not help us anyway since there are going
## to be derived or removed predictors beyond the 
## original set of columns. 

learn.center_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  means <- vapply(data[, col_names], mean, c(mean = 0), na.rm = TRUE)
  step_center_new(terms = x$terms, role = x$role, means = means)
}

process.center_step <- function(x, data, ...) {
  data[, names(x$means)] <- sweep(as.matrix(data[, names(x$means)]), 2, x$means, "-")
  as_tibble(data)
}

print.center_step <- function(x, form_width = 30, ...) {
  cat("Centering with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$means)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

