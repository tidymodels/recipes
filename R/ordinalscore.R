#' Convert Ordinal Factors to Numeric Scores
#'
#' `step_ordinalscore` creates a *specification* of a
#'  recipe step that will convert ordinal factor variables into
#'  numeric scores.
#'
#' @inheritParams step_center
#' @param columns A character string of variables that will be
#'  converted. This is `NULL` until computed by
#'  [prep.recipe()].
#' @param convert A function that takes an ordinal factor vector
#'  as an input and outputs a single numeric variable.
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details Dummy variables from ordered factors with `C`
#'  levels will create polynomial basis functions with `C-1`
#'  terms. As an alternative, this step can be used to translate the
#'  ordered levels into a single numeric vector of values that
#'  represent (subjective) scores. By default, the translation uses
#'  a linear scale (1, 2, 3, ... `C`) but custom score
#'  functions can also be used (see the example below).
#'
#' When you [`tidy()`] this step, a tibble with column `terms` (the
#'  columns that will be affected) is returned.
#'
#' @examples
#' fail_lvls <- c("meh", "annoying", "really_bad")
#'
#' ord_data <-
#'   data.frame(item = c("paperclip", "twitter", "airbag"),
#'              fail_severity = factor(fail_lvls,
#'                                     levels = fail_lvls,
#'                                     ordered = TRUE))
#'
#' model.matrix(~fail_severity, data = ord_data)
#'
#' linear_values <- recipe(~ item + fail_severity, data = ord_data) %>%
#'   step_dummy(item) %>%
#'   step_ordinalscore(fail_severity)
#'
#' linear_values <- prep(linear_values, training = ord_data)
#'
#' bake(linear_values, new_data = NULL, everything())
#'
#' custom <- function(x) {
#'   new_values <- c(1, 3, 7)
#'   new_values[as.numeric(x)]
#' }
#'
#' nonlin_scores <- recipe(~ item + fail_severity, data = ord_data) %>%
#'   step_dummy(item) %>%
#'   step_ordinalscore(fail_severity, convert = custom)
#'
#' tidy(nonlin_scores, number = 2)
#'
#' nonlin_scores <- prep(nonlin_scores, training = ord_data)
#'
#' bake(nonlin_scores, new_data = NULL, everything())
#'
#' tidy(nonlin_scores, number = 2)

step_ordinalscore <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           convert = as.numeric,
           skip = FALSE,
           id = rand_id("ordinalscore")) {
    add_step(
      recipe,
      step_ordinalscore_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        convert = convert,
        skip = skip,
        id = id
      )
    )
  }

step_ordinalscore_new <-
  function(terms, role, trained, columns, convert, skip, id) {
    step(
      subclass = "ordinalscore",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      convert = convert,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_ordinalscore <-
  function(x, training, info = NULL, ...) {
    col_names <- recipes_eval_select(x$terms, training, info)
    ord_check <-
      vapply(training[, col_names], is.ordered, c(logic = TRUE))
    if (!all(ord_check))
      rlang::abort(
        paste0(
          "Ordinal factor variables should be selected as ",
           "inputs into this step."
          )
        )
    step_ordinalscore_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      columns = col_names,
      convert = x$convert,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_ordinalscore <- function(object, new_data, ...) {
  scores <- lapply(new_data[, object$columns], object$convert)
  for (i in object$columns)
    new_data[, i] <- scores[[i]]
  as_tibble(new_data)
}

print.step_ordinalscore <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Scoring for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_ordinalscore <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
