#' Convert Ordinal Factors to Numeric Scores
#'
#' `step_ordinalscore` creates a *specification* of a
#'  recipe step that will convert ordinal factor variables into
#'  numeric scores.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string of variables that will be
#'  converted. This is `NULL` until computed by
#'  [prep.recipe()].
#' @param convert A function that takes an ordinal factor vector
#'  as an input and outputs a single numeric variable.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected).
#' @keywords datagen
#' @concept preprocessing ordinal_data
#' @export
#' @details Dummy variables from ordered factors with `C`
#'  levels will create polynomial basis functions with `C-1`
#'  terms. As an alternative, this step can be used to translate the
#'  ordered levels into a single numeric vector of values that
#'  represent (subjective) scores. By default, the translation uses
#'  a linear scale (1, 2, 3, ... `C`) but custom score
#'  functions can also be used (see the example below).
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
#' linear_values <- prep(linear_values, training = ord_data, retain = TRUE)
#'
#' juice(linear_values, everything())
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
#' nonlin_scores <- prep(nonlin_scores, training = ord_data, retain = TRUE)
#'
#' juice(nonlin_scores, everything())
#'
#' tidy(nonlin_scores, number = 2)

step_ordinalscore <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           convert = as.numeric) {
    add_step(
      recipe,
      step_ordinalscore_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        columns = columns,
        convert = convert
      )
    )
  }

step_ordinalscore_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           convert = NULL) {
    step(
      subclass = "ordinalscore",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      convert = convert
    )
  }

#' @export
prep.step_ordinalscore <-
  function(x, training, info = NULL, ...) {
    col_names <- terms_select(x$terms, info = info)
    ord_check <-
      vapply(training[, col_names], is.ordered, c(logic = TRUE))
    if (!all(ord_check))
      stop("Ordinal factor variables should be selected as ",
           "inputs into this step.",
           call. = TRUE)
    step_ordinalscore_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      columns = col_names,
      convert = x$convert
    )
  }

#' @export
bake.step_ordinalscore <- function(object, newdata, ...) {
  scores <- lapply(newdata[, object$columns], object$convert)
  for (i in object$columns)
    newdata[, i] <- scores[[i]]
  as_tibble(newdata)
}

print.step_ordinalscore <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Scoring for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_ordinalscore
#' @param x A `step_ordinalscore` object.
tidy.step_ordinalscore <- function(x, ...) {
  simple_terms(x, ...)
}
