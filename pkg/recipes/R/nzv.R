step_nzv <- function(recipe, terms, role = NA) {
  add_step(recipe, step_nzv_new(terms = terms, role = role))
}

step_nzv_new <- function(terms = NULL, 
                         role = NA,
                         options = list(freqCut = 95 / 5, uniqueCut = 10, names = TRUE),
                         removals = NULL) {
  step(
    subclass = "nzv", 
    terms = terms,
    role = role,
    options = options,
    removals = removals
  )
}

learn.nzv_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  data <- data[, col_names]
  filter <- do.call("nearZeroVar", c(list(x = data), x$options))
  step_nzv_new(
    terms = x$terms, 
    role = x$role,
    options = x$options,
    removals = filter
  )
}

process.nzv_step <- function(x, data, ...) {
  if(length(x$removals) > 0)
    data <- data[, !(colnames(data) %in% x$removals)]
  as_tibble(data)
}

print.nzv_step <- function(x, form_width = 30, ...) {
  cat("Near-zero variance filter on ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$means)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}

