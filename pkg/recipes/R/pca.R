step_pca_new <- function(terms = NULL, 
                         role = "predictor",
                         num  = 5, 
                         options = list(center = TRUE, scale. = TRUE),
                         object = NULL) {
  
  step(
    subclass = "pca",
    terms = terms,
    role = role, 
    num = num,
    object = object
  )
}

step_pca <- function(recipe, terms, role = "predictor") {
  add_step(recipe, step_pca_new(terms = terms, role = role))
}

learn.pca_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  dat <- data[, col_names]
  prc <- do.call("prcomp", c(list(x = dat), x$options))
  
  step_pca_new(
    terms = x$terms,
    role = x$role,
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


print.pca_step <- function(x, form_width = 30, ...) {
  cat("PCA extraction with ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$object)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}
