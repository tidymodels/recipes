#' Convert factors to strings
#'
#' `step_factor2string()` creates a *specification* of a recipe step that will
#' convert one or more factor vectors to strings.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' [recipe()] has an option `strings_as_factors` that defaults to `TRUE`. If
#' this step is used with the default option, the strings produced by this step
#' will not be converted to factors.
#'
#' Remember that categorical data that will be directly passed to a model should
#' be encoded as factors. This step is helpful for ancillary columns (such as
#' identifiers) that will not be computed on in the model.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' rec <- recipe(~ city + zip, data = Sacramento)
#'
#' make_string <- rec |>
#'   step_factor2string(city)
#'
#' make_string <- prep(make_string,
#'   training = Sacramento,
#'   strings_as_factors = FALSE
#' )
#'
#' make_string
#'
#' # note that `city` is a string in recipe output
#' bake(make_string, new_data = NULL) |> head()
#'
#' # ...but remains a factor in the original data
#' Sacramento |> head()
step_factor2string <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = FALSE,
    skip = FALSE,
    id = rand_id("factor2string")
  ) {
    add_step(
      recipe,
      step_factor2string_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_factor2string_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "factor2string",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_factor2string <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("factor", "ordered"))

  step_factor2string_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_factor2string <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- as.character(new_data[[col_name]])
  }

  new_data
}

#' @export
print.step_factor2string <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Character variables from "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_factor2string <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
