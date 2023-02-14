#' Detect a regular expression
#'
#' `step_regex` creates a *specification* of a recipe step that will
#'   create a new dummy variable based on a regular expression.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... A single selector function to choose which variable
#'  will be searched for the regex pattern. The selector should resolve
#'  to a single variable. See [selections()] for more details.
#' @param pattern A character string containing a regular
#'  expression (or character string for `fixed = TRUE`) to be
#'  matched in the given character vector. Coerced by
#'  `as.character` to a character string if possible.
#' @param options A list of options to [grepl()] that
#'  should not include `x` or `pattern`.
#' @param result A single character value for the name of the new
#'  variable. It should be a valid column name.
#' @param input A single character value for the name of the
#'  variable being searched. This is `NULL` until computed by
#'  [prep()].
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected) and `result` (the
#' new column name) is returned.
#'
#' @template case-weights-not-supported
#'
#' @family dummy variable and encoding steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' data(covers, package = "modeldata")
#'
#' rec <- recipe(~description, covers) %>%
#'   step_regex(description, pattern = "(rock|stony)", result = "rocks") %>%
#'   step_regex(description, pattern = "ratake families")
#'
#' rec2 <- prep(rec, training = covers)
#' rec2
#'
#' with_dummies <- bake(rec2, new_data = covers)
#' with_dummies
#' tidy(rec, number = 1)
#' tidy(rec2, number = 1)
step_regex <- function(recipe,
                       ...,
                       role = "predictor",
                       trained = FALSE,
                       pattern = ".",
                       options = list(),
                       result = make.names(pattern),
                       input = NULL,
                       skip = FALSE,
                       id = rand_id("regex")) {
  if (!is_tune(pattern)) {
    if (!is.character(pattern)) {
      rlang::abort("`pattern` should be a character string")
    }
    if (length(pattern) != 1) {
      rlang::abort("`pattern` should be a single pattern")
    }
  }
  valid_args <- names(formals(grepl))[-(1:2)]
  if (any(!(names(options) %in% valid_args))) {
    rlang::abort(paste0(
      "Valid options are: ",
      paste0(valid_args, collapse = ", ")
    ))
  }

  terms <- enquos(...)
  if (length(terms) > 1) {
    rlang::abort("For this step, at most a single selector can be used.")
  }

  add_step(
    recipe,
    step_regex_new(
      terms = terms,
      role = role,
      trained = trained,
      pattern = pattern,
      options = options,
      result = result,
      input = input,
      skip = skip,
      id = id
    )
  )
}

step_regex_new <-
  function(terms, role, trained, pattern, options, result, input, skip, id) {
    step(
      subclass = "regex",
      terms = terms,
      role = role,
      trained = trained,
      pattern = pattern,
      options = options,
      result = result,
      input = input,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_regex <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_name], types = c("string", "factor", "ordered"))

  if (length(col_name) > 1) {
    rlang::abort("The selector should select at most a single variable")
  }

  step_regex_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    pattern = x$pattern,
    options = x$options,
    input = col_name,
    result = x$result,
    skip = x$skip,
    id = x$id
  )
}

bake.step_regex <- function(object, new_data, ...) {
  if (length(object$input) == 0) {
    # Handle empty selection by adding an all `0` column
    new_data[[object$result]] <- rep(0, times = nrow(new_data))
    return(new_data)
  }

  check_new_data(object$input, object, new_data)

  ## sub in options
  regex <- expr(
    grepl(
      x = getElement(new_data, object$input),
      pattern = object$pattern,
      ignore.case = FALSE,
      perl = FALSE,
      fixed = FALSE,
      useBytes = FALSE
    )
  )
  if (length(object$options) > 0) {
    regex <- rlang::call_modify(regex, !!!object$options)
  }

  new_data[, object$result] <- ifelse(eval(regex), 1L, 0L)
  new_data
}

print.step_regex <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Regular expression dummy variable using "
    pattern <- glue::glue("\"{x$pattern}\"")
    untrained_terms <- rlang::parse_quos(pattern, rlang::current_env())
    print_step(pattern, untrained_terms, x$trained, title, width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_regex <- function(x, ...) {
  term_names <- sel2char(x$terms)
  p <- length(term_names)
  if (is_trained(x)) {
    res <- tibble(
      terms = term_names,
      result = rep(unname(x$result), p)
    )
  } else {
    res <- tibble(
      terms = term_names,
      result = rep(na_chr, p)
    )
  }
  res$id <- x$id
  res
}
