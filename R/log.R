#' Logarithmic Transformation
#'
#' `step_log` creates a *specification* of a recipe step
#'  that will log transform data.
#'
#' @inheritParams step_center
#' @param base A numeric value for the base.
#' @param offset An optional value to add to the data prior to
#'  logging (to avoid `log(0)`).
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param signed A logical indicating whether to take the signed log.
#'  This is sign(x) * abs(log(x)) when abs(x) => 1 or 0 if abs(x) < 1.
#'  If `TRUE` the `offset` argument will be ignored.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the columns that will be affected) and `base`.
#' @examples
#' set.seed(313)
#' examples <- matrix(exp(rnorm(40)), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' log_trans <- rec  %>%
#'   step_log(all_numeric_predictors())
#'
#' log_obj <- prep(log_trans, training = examples)
#'
#' transformed_te <- bake(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(log_trans, number = 1)
#' tidy(log_obj, number = 1)
#'
#' # using the signed argument with negative values
#'
#' examples2 <- matrix(rnorm(40, sd = 5), ncol = 2)
#' examples2 <- as.data.frame(examples2)
#'
#' recipe(~ V1 + V2, data = examples2) %>%
#'   step_log(all_numeric_predictors()) %>%
#'   prep(training = examples2) %>%
#'   bake(examples2)
#'
#' recipe(~ V1 + V2, data = examples2) %>%
#'   step_log(all_numeric_predictors(), signed = TRUE) %>%
#'   prep(training = examples2) %>%
#'   bake(examples2)
#'
step_log <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           base = exp(1),
           offset = 0,
           columns = NULL,
           skip = FALSE,
           signed = FALSE,
           id = rand_id("log")
           ) {
    add_step(
      recipe,
      step_log_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        base = base,
        offset = offset,
        columns = columns,
        skip = skip,
        signed = signed,
        id = id
      )
    )
  }

step_log_new <-
  function(terms, role, trained, base, offset, columns, skip, signed, id) {
    step(
      subclass = "log",
      terms = terms,
      role = role,
      trained = trained,
      base = base,
      offset = offset,
      columns = columns,
      skip = skip,
      signed = signed,
      id = id
    )
  }

#' @export
prep.step_log <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  step_log_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    base = x$base,
    offset = x$offset,
    columns = col_names,
    skip = x$skip,
    signed = x$signed,
    id = x$id
  )
}

#' @export
bake.step_log <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat
  if(all(names(object) != "offset"))
    object$offset <- 0

  if (!object$signed){
    for (i in seq_along(col_names))
      new_data[, col_names[i]] <-
        log(new_data[[ col_names[i] ]] + object$offset, base = object$base)
  } else {
    if (object$offset != 0)
      rlang::warn("When signed is TRUE, offset will be ignored")
     for (i in seq_along(col_names))
       new_data[, col_names[i]] <-
         ifelse(abs(new_data[[ col_names[i] ]]) < 1,
                0,
                sign(new_data[[ col_names[i] ]]) *
                  log(abs(new_data[[ col_names[i] ]]), base = object$base ))
  }
  as_tibble(new_data)
}

print.step_log <-
  function(x, width = max(20, options()$width - 31), ...) {
    msg <- ifelse(x$signed, "Signed log", "Log")
    title <- glue::glue("{msg} transformation on ")
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_log <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$base <- x$base
  out$id <- x$id
  out
}
