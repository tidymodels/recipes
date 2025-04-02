spline2_create <- function(
  x,
  nm = "pred",
  .fn = "bSpline",
  df = 3,
  complete_set = TRUE,
  degree = NULL,
  fn_opts = NULL,
  call = rlang::caller_env()
) {
  vals <- c(
    "bSpline",
    "cSpline",
    "iSpline",
    "mSpline",
    "naturalSpline",
    "bernsteinPoly"
  )
  .fn <- rlang::arg_match(.fn, vals)
  fn_opts <- c(fn_opts, degree = degree)

  if (.fn != "bernsteinPoly" && isTRUE(degree > (df - complete_set))) {
    if (complete_set) {
      cli::cli_abort(
        "{.arg degree} ({degree}) must be less than to {.arg deg_free} \\
        ({df}) when {.code complete_set = FALSE}.",
        call = call
      )
    } else {
      cli::cli_abort(
        "{.arg degree} ({degree}) must be less than or equal to {.arg deg_free} \\
        ({df}) when {.code complete_set = TRUE}.",
        call = call
      )
    }
  }

  .cl <-
    rlang::call2(
      .fn,
      .ns = "splines2",
      x = rlang::expr(x),
      df = df,
      intercept = complete_set,
      !!!fn_opts
    )
  res <- try_fetch_eval_tidy(rlang::eval_tidy(.cl))

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

  if (NROW(new_data) == 0) {
    new_data <- object$Boundary.knots[[1]]
  }

  .cl <- rlang::call2(
    .ns = .ns,
    .fn = .fn,
    !!!object,
    x = rlang::expr(new_data)
  )
  res <- rlang::eval_tidy(.cl)

  if (NROW(new_data) == 0) {
    res <- res[0, , drop = FALSE]
  }

  attributes(res) <- list(dim = dim(res), dimnames = dimnames(res))
  if (length(new_data) == 1) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(res))
  }
  colnames(res) <- names0(ncol(res), paste0(nm, "_"))
  tibble::as_tibble(res)
}
