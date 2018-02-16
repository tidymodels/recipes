#' Convert Ordered Factors to Unordered Factors
#'
#' `step_unorder` creates a *specification* of a recipe
#'  step that will transform the data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected).
#' @keywords datagen
#' @concept preprocessing ordinal_data
#' @export
#' @details The factors level order is preserved during the transformation.
#' @examples
#' lmh <- c("Low", "Med", "High")
#'
#' examples <- data.frame(X1 = factor(rep(letters[1:4], each = 3)),
#'                        X2 = ordered(rep(lmh, each = 4),
#'                                     levels = lmh))
#'
#' rec <- recipe(~ X1 + X2, data = examples)
#'
#' factor_trans <- rec  %>%
#'   step_unorder(all_predictors())
#'
#' factor_obj <- prep(factor_trans, training = examples)
#'
#' transformed_te <- bake(factor_obj, examples)
#' table(transformed_te$X2, examples$X2)
#'
#' tidy(factor_trans, number = 1)
#' tidy(factor_obj, number = 1)
#' @seealso [step_ordinalscore()] [recipe()]
#' [prep.recipe()] [bake.recipe()]

step_unorder <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE) {
    add_step(recipe,
             step_unorder_new(
               terms = ellipse_check(...),
               role = role,
               trained = trained,
               columns = columns,
               skip = skip
             ))
  }

step_unorder_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE) {
    step(
      subclass = "unorder",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip
    )
  }

#' @export
prep.step_unorder <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  order_check <- vapply(training[, col_names],
                        is.ordered,
                        logical(1L))
  if(all(!order_check)) {
    stop("`step_unorder` required ordered factors.", call. = FALSE)
  } else {
    if(any(!order_check)) {
      bad_cols <- names(order_check)[!order_check]
      bad_cols <- paste0(bad_cols, collapse = ", ")
      warning("`step_unorder` requires ordered factors. Variables ",
              bad_cols,
              " will be ignored.", call. = FALSE)
      col_names <- names(order_check)[order_check]
    }
  }

  step_unorder_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip
  )
}

#' @importFrom tibble as_tibble
#' @export
bake.step_unorder <- function(object, newdata, ...) {
  for (i in seq_along(object$columns))
    newdata[, object$columns[i]] <-
      factor(as.character(getElement(newdata, object$columns[i])),
             levels = levels(getElement(newdata, object$columns[i])))
  as_tibble(newdata)
}


print.step_unorder <-
  function(x, width = max(20, options()$width - 33), ...) {
    cat("Unordered variables ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_unorder
#' @param x A `step_unorder` object.
tidy.step_unorder <- function(x, ...) {
  simple_terms(x, ...)
}
