#' Create counts of patterns using regular expressions
#'
#' `step_count()` creates a *specification* of a recipe step that will create a
#' variable that counts instances of a regular expression pattern in text.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @inheritParams step_dummy
#' @param ... One or more selector functions to choose which variables will be
#'   searched for the regex pattern. See [selections()] for more details.
#' @param pattern A character string containing a regular expression (or
#'   character string for `fixed = TRUE`) to be matched in the given character
#'   vector. Coerced by `as.character` to a character string if possible.
#' @param normalize A logical; should the integer counts be divided by the total
#'   number of characters in the string?.
#' @param options A list of options to [gregexpr()] that should not include `x`
#'   or `pattern`.
#' @param result A single character value for the name of the new variable. It
#'   should be a valid column name. If the selection resolves to more than one
#'   variable, the new variables are named `{variable}_{result}` instead.
#' @param input The names of the variables being searched. This is `NULL` until
#'   computed by [prep()].
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `result` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{result}{character, the new column names}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-creation
#'
#' @template case-weights-not-supported
#'
#' @family dummy variable and encoding steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' data(covers, package = "modeldata")
#'
#' rec <- recipe(~description, covers) |>
#'   step_count(description, pattern = "(rock|stony)", result = "rocks") |>
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
#'
#' # When more than one variable is selected, the new variables are named
#' # `{variable}_{result}`
#' covers$description2 <- rev(covers$description)
#'
#' recipe(~., covers) |>
#'   step_count(description, description2, pattern = "rock", result = "rocks") |>
#'   prep() |>
#'   bake(new_data = NULL)
step_count <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  pattern = ".",
  normalize = FALSE,
  options = list(),
  result = make.names(pattern),
  input = NULL,
  sparse = "auto",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("count")
) {
  check_string(pattern)

  valid_args <- names(formals(grepl))[-(1:2)]
  if (any(!(names(options) %in% valid_args))) {
    cli::cli_abort(
      c(
        "x" = "The following elements of {.arg options} are not allowed:",
        "*" = "{.val {setdiff(names(options), valid_args)}}.",
        "i" = "Valid options are: {.val {valid_args}}."
      )
    )
  }

  add_step(
    recipe,
    step_count_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      pattern = pattern,
      normalize = normalize,
      options = options,
      result = result,
      input = input,
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_count_new <-
  function(
    terms,
    role,
    trained,
    pattern,
    normalize,
    options,
    result,
    input,
    sparse,
    keep_original_cols,
    skip,
    id
  ) {
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
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_count <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))
  check_string(x$pattern, allow_empty = TRUE, arg = "pattern")
  check_string(x$result, allow_empty = FALSE, arg = "result")
  check_bool(x$normalize, arg = "normalize")
  check_sparse_arg(x$sparse)
  check_options(x$options, exclude = c("x", "pattern"))

  step_count_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    pattern = x$pattern,
    normalize = x$normalize,
    options = x$options,
    input = col_names,
    result = x$result,
    sparse = x$sparse,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_count <- function(object, new_data, ...) {
  col_names <- names(object$input)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0L) {
    return(new_data)
  }

  new_names <- multi_result_names(col_names, object$result)

  cols <- list()

  for (i in seq_along(col_names)) {
    ## sub in options
    regex <- expr(
      gregexpr(
        text = new_data[[col_names[i]]],
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

    res <- vapply(eval(regex), counter, integer(1))

    if (object$normalize) {
      totals <- nchar(as.character(new_data[[col_names[i]]]))
      res <- res / totals
    }

    if (sparse_is_yes(object$sparse)) {
      if (object$normalize) {
        res <- sparsevctrs::as_sparse_double(res)
      } else {
        res <- sparsevctrs::as_sparse_integer(res)
      }
    }

    cols[[new_names[i]]] <- res
  }

  cols <- tibble::new_tibble(cols, nrow = nrow(new_data))

  cols <- check_name(cols, new_data, object, new_names)
  new_data <- vec_cbind(new_data, cols, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

counter <- function(x) length(x[x > 0])

#' @export
print.step_count <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Regular expression counts using "
    print_step(x$input, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_count <- function(x, ...) {
  if (is_trained(x)) {
    col_names <- names(x$input)
    res <- tibble(
      terms = col_names,
      result = multi_result_names(col_names, unname(x$result))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      result = rep(na_chr, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @export
.recipes_estimate_sparsity.step_count <- function(x, data, ...) {
  lapply(
    seq_len(ncol(data)),
    function(x) c(n_cols = 1, sparsity = 0.5)
  )
}
