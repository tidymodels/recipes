#' Create an indicator of missingness
#'
#' `step_indicate` creates a *specification* of a recipe step that
#'   will add new columns to a dataset that indicate missingness. Optionally
#'   removes the original column.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param remove A logical. Should the specified columns be removed from the
#'   data after indicating missingness? Defaults to `TRUE`.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param numeric A logical indicating if missingness should be indicated with
#'   a numeric vector of zeros and ones, or with logical values. Defaults to
#'   `TRUE`, which corresponds to numeric indicators.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @details Uses [is.na()] to determine if observations are missing. This means
#'   that `NaN` values are also treated as missing.
#' @export
#'
#' @examples
#'
#' i <- iris
#' diag(i) <- NA
#'
#' recipe(~ Sepal.Width + Sepal.Length, data = i) %>%
#'   step_indicate(Sepal.Width) %>%
#'   step_indicate(Sepal.Length, remove = FALSE) %>%
#'   prep() %>%
#'   juice()
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()] [step_naomit()]
#'   [step_sentinel()] [is.na()]
step_indicate <-
  function(recipe,
           ...,
           remove = TRUE,
           role = "predictor",
           trained = FALSE,
           numeric = TRUE,
           skip = FALSE) {
    # TODO: something about a random id?

    add_step(
      recipe,
      step_indicate_new(
        terms = ellipse_check(...),
        remove = remove,
        role = role,
        trained = trained,
        numeric = numeric,
        skip = skip
      )
    )
  }

step_indicate_new <-
  function(terms = NULL,
           remove = TRUE,
           role = "predictor",
           trained = FALSE,
           numeric = TRUE,
           skip = FALSE) {
    step(
      subclass = "indicate",
      terms = terms,
      remove = remove,
      role = role,
      trained = trained,
      numeric = numeric,
      skip = skip)
  }

#' @export
prep.step_indicate <- function(x, training, info = NULL, ...) {
  x$columns <- terms_select(x$terms, info = info)
  x$trained <- TRUE
  x
}

indicate_helper <- function(x) as.numeric(is.na(x))

#' @export
bake.step_indicate <- function(object, new_data, ...) {

  for (col in object$columns) {
    new_col <- paste0(col, "_missing")
    new_data <- mutate(new_data, !!new_col := indicate_helper(!!sym(col)))
  }

  if (object$remove)
    new_data <- select(new_data, -one_of(object$columns))

  new_data
}

# TODO: what's the current standard for this?
print.step_indicate <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Indicating missingness for ",  sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }
