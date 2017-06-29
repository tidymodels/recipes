#' Convert Ordinal Factors to Numeric Scores
#'
#' \code{step_ordinalscore} creates a \emph{specification} of a recipe step that
#'   will convert ordinal factor variables into numeric scores.
#'
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created.
#' @param vars A character string of variables that will be converted. This is \code{NULL}
#'   until computed by \code{\link{prepare.recipe}}.
#' @param convert A function that takes an ordinal factor vector as an input and outputs a single numeric variable.
#' @return \code{step_ordinalscore} returns an object of class \code{step_ordinalscore}.
#' @keywords datagen
#' @concept preprocessing ordinal_data
#' @export
#' @details Dummy variables from ordered factors with \code{C} levels will create polynomial basis functions with \code{C-1} terms. As an alternative, this step can be used to translate the ordered levels into a single numeric vector of values that represent (subjective) scores. By default, the translation uses a linear scale (1, 2, 3, ... \code{C}) but custom score functions can also be used (see the example below). 
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
#' linear_values <- prepare(linear_values, training = ord_data, retain = TRUE)
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
#' nonlin_scores <- prepare(nonlin_scores, training = ord_data, retain = TRUE)
#' 
#' juice(nonlin_scores, everything())

step_ordinalscore <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           vars = NULL,
           convert = as.numeric) {
    add_step(
      recipe,
      step_ordinalscore_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        vars = vars,
        convert = convert
      )
    )
  }

step_ordinalscore_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           vars = NULL,
           convert = NULL) {
    step(
      subclass = "ordinalscore",
      terms = terms,
      role = role,
      trained = trained,
      vars = vars,
      convert = convert
    )
  }

#' @export
prepare.step_ordinalscore <-
  function(x, training, info = NULL, ...) {
    col_names <- select_terms(x$terms, info = info)
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
      vars = col_names,
      convert = x$convert
    )
  }

#' @export
bake.step_ordinalscore <- function(object, newdata, ...) {
  scores <- lapply(newdata[, object$vars], object$convert)
  for (i in object$vars)
    newdata[, i] <- scores[[i]]
  as_tibble(newdata)
}

print.step_ordinalscore <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Scoring for ", sep = "")
    if (x$trained) {
      cat(format_ch_vec(names(x$vars), width = width))
    } else
      cat(format_selectors(x$terms, wdth = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
