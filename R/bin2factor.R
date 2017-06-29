#' Create a Factors from A Dummy Variable
#'
#' \code{step_bin2factor} creates a \emph{specification} of a recipe step that
#' will create a two-level factor from a single dummy variable.
#' @inheritParams step_center
#' @param ... Selector functions that choose which variables will be converted.
#'   See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param levels A length 2 character string that indicate the factor levels
#' for the 1's (in the first position) and the zeros (second)
#' @param objects A vector with the selected variable names. This is
#' \code{NULL} until computed by \code{\link{prepare.recipe}}.
#' @return \code{step_bin2factor}  returns an object of class
#'   \code{step_bin2factor}. The variables selected will be replaced by their
#'   factor doppelgängers by  \code{\link{bake.recipe}}.
#' @details This operation may be useful for situations where a binary piece of
#'   information may need to be represented as categorical instead of numeric.
#'   For example, naive Bayes models would do better to have factor predictors
#'   so that the binomial distribution is modeled in stead of a Gaussian
#'   probability density of numeric binary data.
#' Note that the numeric data is only verified to be numeric (and does not
#' count levels).
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
#' rec <- prepare(rec, training = covers)
#' results <- bake(rec, newdata = covers)
#'
#' table(results$rocks, results$more_rocks)
step_bin2factor <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           levels = c("yes", "no"),
           objects = NULL) {
    if (length(levels) != 2 | !is.character(levels))
      stop("`levels` should be a two element character string", call. = FALSE)
    add_step(
      recipe,
      step_bin2factor_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        levels = levels,
        objects = objects
      )
    )
  }

step_bin2factor_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           levels = NULL,
           objects = NULL) {
    step(
      subclass = "bin2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      objects = objects
    )
  }

#' @export
prepare.step_bin2factor <- function(x, training, info = NULL, ...) {
  col_names <- select_terms(x$terms, info = info)
  if (length(col_names) < 1)
    stop("The selector should only select at least one variable")
  if (any(info$type[info$variable %in% col_names] != "numeric"))
    stop("The variables should be numeric")
  step_bin2factor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    levels = x$levels,
    objects = col_names
  )
}

bake.step_bin2factor <- function(object, newdata, ...) {
  for (i in seq_along(object$objects))
    newdata[, object$objects[i]] <-
      factor(ifelse(
        getElement(newdata, object$objects[i]) == 1,
        object$levels[1],
        object$levels[2]
      ),
      levels = object$levels)
  newdata
}

print.step_bin2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Dummy variable to factor conversion for ", sep = "")
    if (x$trained) {
      cat(format_ch_vec(x$objects, width = width))
    } else
      cat(format_selectors(x$terms, wdth = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
