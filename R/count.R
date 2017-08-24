#' Create Counts of Patterns using Regular Expressions
#'
#' \code{step_count} creates a \emph{specification} of a recipe
#'  step that will create a variable that counts instances of a
#'  regular expression pattern in text.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... A single selector functions to choose which variable
#'  will be searched for the pattern. The selector should resolve
#'  into a single variable. See \code{\link{selections}} for more
#'  details.
#' @param role For a variable created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new dummy variable column created by the original
#'  variable will be used as a predictors in a model.
#' @param pattern A character string containing a regular
#'  expression (or character string for \code{fixed = TRUE}) to be
#'  matched in the given character vector. Coerced by
#'  \code{as.character} to a character string if possible.
#' @param normalize A logical; should the integer counts be
#'  divided by the total number of characters in the string?.  
#' @param options A list of options to \code{\link{gregexpr}} that
#'  should not include \code{x} or \code{pattern}.
#' @param result A single character value for the name of the new
#'  variable. It should be a valid column name.
#' @param input A single character value for the name of the
#'  variable being searched. This is \code{NULL} until computed by
#'  \code{\link{prep.recipe}}.
#' @keywords datagen
#' @concept preprocessing dummy_variables regular_expressions
#' @export
#' @examples
#' data(covers)
#'
#' rec <- recipe(~ description, covers) %>%
#'   step_count(description, pattern = "(rock|stony)", result = "rocks") %>%
#'   step_count(description, pattern = "famil", normalize = TRUE)
#'
#' rec2 <- prep(rec, training = covers)
#' rec2
#'
#' count_values <- bake(rec2, newdata = covers)
#' count_values
step_count <- function(recipe,
                       ...,
                       role = "predictor",
                       trained = FALSE,
                       pattern = ".",
                       normalize = FALSE,
                       options = list(),
                       result = make.names(pattern),
                       input = NULL) {
  if (!is.character(pattern))
    stop("`pattern` should be a character string", call. = FALSE)
  if (length(pattern) != 1)
    stop("`pattern` should be a single pattern", call. = FALSE)
  valid_args <- names(formals(grepl))[- (1:2)]
  if (any(!(names(options) %in% valid_args)))
    stop("Valid options are: ",
         paste0(valid_args, collapse = ", "),
         call. = FALSE)
  
  terms <- check_ellipses(...)
  if (length(terms) > 1)
    stop("For this step, only a single selector can be used.", call. = FALSE)
  
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
      input = input
    )
  )
}

step_count_new <- function(terms = NULL,
                           role = NA,
                           trained = FALSE,
                           pattern = NULL,
                           normalize = NULL,
                           options = NULL,
                           result = NULL,
                           input = NULL) {
  step(
    subclass = "count",
    terms = terms,
    role = role,
    trained = trained,
    pattern = pattern,
    normalize = normalize,
    options = options,
    result = result,
    input = input
  )
}

#' @export
prep.step_count <- function(x, training, info = NULL, ...) {
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    stop("The selector should only select a single variable")
  if (any(info$type[info$variable %in% col_name] != "nominal"))
    stop("The regular expression input should be character or factor")
  
  step_count_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    pattern = x$pattern,
    normalize = x$normalize,
    options = x$options,
    input = col_name,
    result = x$result
  )
}

#' @importFrom rlang expr
bake.step_count <- function(object, newdata, ...) {
  ## sub in options
  regex <- expr(
    gregexpr(
      text = getElement(newdata, object$input),
      pattern = object$pattern,
      ignore.case = FALSE,
      perl = FALSE,
      fixed = FALSE,
      useBytes = FALSE
    )
  )
  if (length(object$options) > 0)
    regex <- mod_call_args(regex, args = object$options)
  
  newdata[, object$result] <- vapply(eval(regex), counter, integer(1))
  if(object$normalize) {
    totals <- nchar(as.character(getElement(newdata, object$input)))
    newdata[, object$result] <- newdata[, object$result]/totals
  }
  newdata
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
