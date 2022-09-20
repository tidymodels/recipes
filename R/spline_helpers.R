spline2_create <- function(x, nm = "pred", .fn = "bSpline", df = 3, fn_opts = NULL) {
  vals <- c("bSpline", "cSpline", "iSpline", "mSpline", "naturalSpline", "bernsteinPoly")
  .fn <- rlang::arg_match(.fn, vals)
  df <- max(df, 2)

  .cl <- rlang::call2(.fn, .ns = "splines2", x = rlang::expr(x), df = df, !!!fn_opts)
  res <- try(rlang::eval_tidy(.cl), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(NULL)
  }
  res <- attributes(res)
  res$x <- NULL
  res$class <- NULL
  res$dimnames <- NULL
  res$.fn <- .fn
  res$.ns = "splines2"
  res$nm <- nm
  res
}

spline2_apply <- function(object, new_data) {
  .ns <- object$.ns
  .fn <- object$.fn
  nm <- object$nm
  object$.ns <- NULL
  object$.fn <- NULL
  object$nm <- NULL
  .cl <- rlang::call2(.ns = .ns, .fn = .fn, !!!object, x = rlang::expr(new_data))
  res <- rlang::eval_tidy(.cl)
  res <- apply(res, 2, I)
  colnames(res) <- names0(ncol(res), paste0(nm, "_"))
  tibble::as_tibble(res)
}
