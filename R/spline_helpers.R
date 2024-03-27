spline2_create <- function(x, nm = "pred", .fn = "bSpline", df = 3, complete_set = TRUE,
                           degree = NULL, fn_opts = NULL) {
  vals <- c("bSpline", "cSpline", "iSpline", "mSpline", "naturalSpline", "bernsteinPoly")
  .fn <- rlang::arg_match(.fn, vals)
  fn_opts <- c(fn_opts, degree = degree)

  .cl <-
    rlang::call2(
      .fn,
      .ns = "splines2",
      x = rlang::expr(x),
      df = df,
      intercept = complete_set,
      !!!fn_opts
    )
  res <- try(rlang::eval_tidy(.cl), silent = TRUE)
  if (inherits(res, "try-error")) {
    spline_msg(res)
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

spline_msg <- function(x) {
  x <- as.character(x)
  x <- strsplit(x, "\\n")[[1]]
  x <- paste0(x[-1], collapse = ". ")
  cli::cli_warn(trimws(x, which = "left"))
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
  attributes(res) <- list(dim = dim(res), dimnames = dimnames(res))
  if (length(new_data) == 1) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(res))
  }
  colnames(res) <- names0(ncol(res), paste0(nm, "_"))
  tibble::as_tibble(res)
}
