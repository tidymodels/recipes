#' Create Counts of Patterns using Regular Expressions
#'
#' `step_count` creates a *specification* of a recipe
#'  step that will create a variable that counts instances of a
#'  regular expression pattern in text.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... A single selector function to choose which variable
#'  will be searched for the regex pattern. The selector should
#'  resolve to a single variable. See [selections()] for more details.
#' @param pattern A character string containing a regular
#'  expression (or character string for `fixed = TRUE`) to be
#'  matched in the given character vector. Coerced by
#'  `as.character` to a character string if possible.
#' @param normalize A logical; should the integer counts be
#'  divided by the total number of characters in the string?.
#' @param options A list of options to [gregexpr()] that
#'  should not include `x` or `pattern`.
#' @param result A single character value for the name of the new
#'  variable. It should be a valid column name.
#' @param input A single character value for the name of the
#'  variable being searched. This is `NULL` until computed by
#'  [prep.recipe()].
#' @template step-return
#' @details When you [`tidy()`] this step, a tibble with columns `terms` (the
#'  selectors or variables selected) and `result` (the
#'  new column name) is returned.
#' @family dummy variable and encoding steps
#' @export
#' @examples
#' library(modeldata)
#' data(covers)
#'
#' rec <- recipe(~ description, covers) %>%
#'   step_count(description, pattern = "(rock|stony)", result = "rocks") %>%
#'   step_count(description, pattern = "famil", normalize = TRUE)
#'
#' rec2 <- prep(rec, training = covers)
#' rec2
#'
#' count_values <- bake(rec2, new_data = covers)
#' count_values
#'
#' tidy(rec, number = 1)
#' tidy(rec2, number = 1)
step_count <- function(recipe,
                       ...,
                       role = "predictor",
                       trained = FALSE,
                       pattern = ".",
                       normalize = FALSE,
                       options = list(),
                       result = make.names(pattern),
                       input = NULL,
                       skip = FALSE,
                       id = rand_id("count")) {
  if (!is.character(pattern))
    rlang::abort("`pattern` should be a character string")
  if (length(pattern) != 1)
    rlang::abort("`pattern` should be a single pattern")
  valid_args <- names(formals(grepl))[- (1:2)]
  if (any(!(names(options) %in% valid_args)))
    rlang::abort(paste0("Valid options are: ",
                        paste0(valid_args, collapse = ", ")))

  terms <- ellipse_check(...)
  if (length(terms) > 1)
    rlang::abort("For this step, only a single selector can be used.")

  add_step(
    recipe,
    step_count_new(
      terms = terms,
      role = role,
      trained = trained,
      pattern = pattern,
      normalize = normalize,
      options = options,
      result = result,
      input = input,
      skip = skip,
      id = id
    )
  )
}

step_count_new <-
  function(terms, role, trained, pattern, normalize, options, result, input, skip, id) {
    step(
      subclass = "count",
      terms = terms,
      role = role,
      trained = trained,
      pattern = pattern,
      normalize = normalize,
      options = options,
      result = result,
      input = input,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_count <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)
  if (length(col_name) != 1)
    rlang::abort("The selector should only select a single variable")
  if (any(info$type[info$variable %in% col_name] != "nominal"))
    rlang::abort("The regular expression input should be character or factor")

  step_count_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    pattern = x$pattern,
    normalize = x$normalize,
    options = x$options,
    input = col_name,
    result = x$result,
    skip = x$skip,
    id = x$id
  )
}

bake.step_count <- function(object, new_data, ...) {
  ## sub in options
  regex <- expr(
    gregexpr(
      text = getElement(new_data, object$input),
      pattern = object$pattern,
      ignore.case = FALSE,
      perl = FALSE,
      fixed = FALSE,
      useBytes = FALSE
    )
  )
  if (length(object$options) > 0)
    regex <- mod_call_args(regex, args = object$options)

  new_data[, object$result] <- vapply(eval(regex), counter, integer(1))
  if(object$normalize) {
    totals <- nchar(as.character(getElement(new_data, object$input)))
    new_data[, object$result] <- new_data[, object$result]/totals
  }
  new_data
}

counter <- function(x) length(x[x > 0])


print.step_count <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Regular expression counts using `",
        x$pattern,
        "`",
        sep = "")
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_count <- function(x, ...) {
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
