#' @export
extract_parameter_set_dials.recipe <- function(x, ...) {
  all_args <- generics::tunable(x)
  tuning_param <- generics::tune_args(x)
  res <-
    dplyr::inner_join(
      tuning_param |> dplyr::select(-tunable),
      all_args,
      by = c("name", "source", "component", "component_id")
    )

  objects <- list()
  for (i in seq_len(nrow(res))) {
    objects[[i]] <- eval_call_info(res$call_info[[i]])
  }

  dials::parameters_constr(
    res$name,
    res$id,
    res$source,
    res$component,
    res$component_id,
    objects
  )
}

eval_call_info <- function(x, call) {
  if (!is.null(x)) {
    # Look for other options
    allowed_opts <- c("range", "trans", "values")
    if (any(names(x) %in% allowed_opts)) {
      opts <- x[names(x) %in% allowed_opts]
    } else {
      opts <- list()
    }
    res <- try(
      rlang::eval_tidy(rlang::call2(x$fun, .ns = x$pkg, !!!opts)),
      silent = TRUE
    )
    if (inherits(res, "try-error")) {
      cli::cli_abort(
        "Error when calling {.fn {x$fun}}: {as.character(res)}",
        call = call
      )
    }
  } else {
    res <- NA
  }
  res
}

#' @export
extract_parameter_dials.recipe <- function(x, parameter, ...) {
  extract_parameter_dials(extract_parameter_set_dials(x), parameter)
}
