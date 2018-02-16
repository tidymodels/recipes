#' Create a Factors from A Dummy Variable
#'
#' `step_bin2factor` creates a *specification* of a
#'  recipe step that will create a two-level factor from a single
#'  dummy variable.
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... Selector functions that choose which variables will
#'  be converted. See [selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param levels A length 2 character string that indicate the
#'  factor levels for the 1's (in the first position) and the zeros
#'  (second)
#' @param columns A vector with the selected variable names. This
#'  is `NULL` until computed by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected).
#' @details This operation may be useful for situations where a
#'  binary piece of information may need to be represented as
#'  categorical instead of numeric. For example, naive Bayes models
#'  would do better to have factor predictors so that the binomial
#'  distribution is modeled in stead of a Gaussian probability
#'  density of numeric binary data. Note that the numeric data is
#'  only verified to be numeric (and does not count levels).
#' @keywords datagen
#' @concept preprocessing dummy_variables factors
#' @export
#' @examples
#' data(covers)
#'
#' rec <- recipe(~ description, covers) %>%
#'  step_regex(description, pattern = "(rock|stony)", result = "rocks") %>%
#'  step_regex(description, pattern = "(rock|stony)", result = "more_rocks") %>%
#'  step_bin2factor(rocks)
#'
#' tidy(rec, number = 3)
#'
#' rec <- prep(rec, training = covers)
#' results <- bake(rec, newdata = covers)
#'
#' table(results$rocks, results$more_rocks)
#'
#' tidy(rec, number = 3)
step_bin2factor <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           levels = c("yes", "no"),
           columns = NULL,
           skip = FALSE) {
    if (length(levels) != 2 | !is.character(levels))
      stop("`levels` should be a two element character string", call. = FALSE)
    add_step(
      recipe,
      step_bin2factor_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        levels = levels,
        columns = columns,
        skip = skip
      )
    )
  }

step_bin2factor_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           levels = NULL,
           columns = NULL,
           skip = FALSE) {
    step(
      subclass = "bin2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      columns = columns,
      skip = skip
    )
  }

#' @export
prep.step_bin2factor <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (length(col_names) < 1)
    stop("The selector should only select at least one variable")
  if (any(info$type[info$variable %in% col_names] != "numeric"))
    stop("The variables should be numeric")
  step_bin2factor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    levels = x$levels,
    columns = col_names,
    skip = x$skip
  )
}

bake.step_bin2factor <- function(object, newdata, ...) {
  for (i in seq_along(object$columns))
    newdata[, object$columns[i]] <-
      factor(ifelse(
        getElement(newdata, object$columns[i]) == 1,
        object$levels[1],
        object$levels[2]
      ),
      levels = object$levels)
  newdata
}

print.step_bin2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Dummy variable to factor conversion for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_bin2factor
#' @param x A `step_bin2factor` object.
tidy.step_bin2factor <- function(x, ...) {
  simple_terms(x, ...)
}
