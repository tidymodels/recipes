#' Hyperbolic transformations
#'
#' `step_hyperbolic()` creates a *specification* of a recipe step that will
#' transform data using a hyperbolic function.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param func A character value for the function. Valid values are `"sinh"`,
#'   `"cosh"`, or `"tanh"`.
#' @param inverse A logical: should the inverse function be used?
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `inverse`, `func` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{inverse}{logical, is the inverse function be used}
#'   \item{func}{character, name of function. `"sinh"`, `"cosh"`, or `"tanh"`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examples
#' set.seed(313)
#' examples <- matrix(rnorm(40), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' cos_trans <- rec |>
#'   step_hyperbolic(
#'     all_numeric_predictors(),
#'     func = "cosh", inverse = FALSE
#'   )
#'
#' cos_obj <- prep(cos_trans, training = examples)
#'
#' transformed_te <- bake(cos_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#'
#' tidy(cos_trans, number = 1)
#' tidy(cos_obj, number = 1)
step_hyperbolic <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    func = c("sinh", "cosh", "tanh"),
    inverse = TRUE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("hyperbolic")
  ) {
    if (!is_tune(func)) {
      func <- rlang::arg_match(func)
    }
    add_step(
      recipe,
      step_hyperbolic_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        func = func,
        inverse = inverse,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_hyperbolic_new <-
  function(terms, role, trained, func, inverse, columns, skip, id) {
    step(
      subclass = "hyperbolic",
      terms = terms,
      role = role,
      trained = trained,
      func = func,
      inverse = inverse,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_hyperbolic <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  func <- x$func
  x$func <- rlang::arg_match(
    func,
    c("sinh", "cosh", "tanh"),
    error_arg = "func"
  )
  check_bool(x$inverse, error_arg = "inverse")

  step_hyperbolic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    func = x$func,
    inverse = x$inverse,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_hyperbolic <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  func <- if (object$inverse) {
    get(paste0("a", object$func))
  } else {
    get(object$func)
  }

  for (col_name in col_names) {
    new_data[[col_name]] <- func(new_data[[col_name]])
  }

  new_data
}

#' @export
print.step_hyperbolic <-
  function(x, width = max(20, options()$width - 32), ...) {
    ttl <- paste("Hyperbolic", substr(x$func, 1, 3))
    if (x$inverse) {
      ttl <- paste(ttl, "(inv)")
    }
    title <- glue("{ttl} transformation on ")
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_hyperbolic <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$inverse <- x$inverse
  out$func <- x$func
  out$id <- x$id
  out
}
