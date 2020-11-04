#' Create Dummy Variables using Regular Expressions
#'
#' `step_regex` creates a *specification* of a recipe step that will
#'   create a new dummy variable based on a regular expression.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... A single selector functions to choose which variable
#'  will be searched for the pattern. The selector should resolve
#'  into a single variable. See [selections()] for more
#'  details. For the `tidy` method, these are not currently
#'  used.
#' @param role For a variable created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new dummy variable column created by the original
#'  variable will be used as a predictor in a model.
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
#'  [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `result` (the
#'  new column name).
#' @keywords datagen
#' @concept preprocessing
#' @concept dummy_variables
#' @concept regular_expressions
#' @export
#' @examples
#' library(modeldata)
#' data(covers)
#'
#' rec <- recipe(~ description, covers) %>%
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
  if (!is_tune(pattern) & !is_varying(pattern)) {
    if (!is.character(pattern)) {
      rlang::abort("`pattern` should be a character string")
    }
    if (length(pattern) != 1) {
      rlang::abort("`pattern` should be a single pattern")
    }
  }
  valid_args <- names(formals(grepl))[-(1:2)]
  if (any(!(names(options) %in% valid_args))) {
    rlang::abort(paste0("Valid options are: ",
                        paste0(valid_args, collapse = ", ")))
  }

  terms <- ellipse_check(...)
  if (length(terms) > 1)
    rlang::abort("For this step, only a single selector can be used.")

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
  col_name <- eval_select_recipes(x$terms, training, info)

  if (length(col_name) != 1)
    rlang::abort("The selector should only select a single variable")
  if (any(info$type[info$variable %in% col_name] != "nominal"))
    rlang::abort("The regular expression input should be character or factor")

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
  if (length(object$options) > 0)
    regex <- mod_call_args(regex, args = object$options)

  new_data[, object$result] <- ifelse(eval(regex), 1, 0)
  new_data
}

print.step_regex <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Regular expression dummy variable using `",
        x$pattern,
        "`",
        sep = "")
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


#' @rdname step_regex
#' @param x A `step_regex` object.
#' @export
tidy.step_regex <- function(x, ...) {
  term_names <- sel2char(x$terms)
  p <- length(term_names)
  if (is_trained(x)) {
    res <- tibble(terms = term_names,
                  result = rep(x$result, p))
  } else {
    res <- tibble(terms = term_names,
                  result = rep(na_chr, p))
  }
  res$id <- x$id
  res
}

