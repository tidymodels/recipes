step_scale <- function(recipe, terms, role = NA) {
  add_step(recipe, step_scale_new(terms = terms, role = role))
}

step_scale_new <- function(terms = NULL, role = NA, sds = NULL) {
  step(
    subclass = "scale", 
    terms = terms,
    role = role,
    sds = sds
  )
}

learn.scale_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  sds <- unlist(lapply(data[, col_names], sd, na.rm = TRUE))
  step_scale_new(terms = x$terms, role = x$role, sds)
}

process.scale_step <- function(x, data, ...) {
  data[, names(x$sds)] <- sweep(as.matrix(data[, names(x$sds)]), 2, x$sds, "-")
  as_tibble(data)
}

print.scale_step <- function(x, form_width = 30, ...) {
  cat("Scaling with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$sds)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}
