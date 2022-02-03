#' Create a Factors from A Dummy Variable
#'
#' `step_bin2factor` creates a *specification* of a
#'  recipe step that will create a two-level factor from a single
#'  dummy variable.
#' @inheritParams step_center
#' @param levels A length 2 character string that indicates the
#'  factor levels for the 1's (in the first position) and the zeros
#'  (second)
#' @param ref_first Logical. Should the first level, which replaces
#' 1's, be the factor reference level?
#' @param columns A vector with the selected variable names. This
#'  is `NULL` until computed by [prep()].
#' @template step-return
#' @details This operation may be useful for situations where a
#'  binary piece of information may need to be represented as
#'  categorical instead of numeric. For example, naive Bayes models
#'  would do better to have factor predictors so that the binomial
#'  distribution is modeled instead of a Gaussian probability
#'  density of numeric binary data. Note that the numeric data is
#'  only verified to be numeric (and does not count levels).
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#'  `terms` (the columns that will be affected) is returned.
#'
#' @family dummy variable and encoding steps
#' @export
#' @examples
#' library(modeldata)
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
#' results <- bake(rec, new_data = covers)
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
           ref_first = TRUE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("bin2factor")) {
    if (length(levels) != 2 | !is.character(levels))
      rlang::abort("`levels` should be a two element character string")
    add_step(
      recipe,
      step_bin2factor_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        levels = levels,
        ref_first = ref_first,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_bin2factor_new <-
  function(terms, role, trained, levels, ref_first, columns, skip, id) {
    step(
      subclass = "bin2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      ref_first = ref_first,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_bin2factor <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (any(info$type[info$variable %in% col_names] != "numeric")) {
    rlang::abort("The variables should be numeric")
  }

  step_bin2factor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    levels = x$levels,
    ref_first = x$ref_first,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

bake.step_bin2factor <- function(object, new_data, ...) {
  levs <- if (object$ref_first) object$levels else rev(object$levels)
  for (i in seq_along(object$columns))
    new_data[, object$columns[i]] <-
      factor(ifelse(
        getElement(new_data, object$columns[i]) == 1,
        object$levels[1],
        object$levels[2]
      ),
      levels = levs)
  new_data
}

print.step_bin2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Dummy variable to factor conversion for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_bin2factor <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}
