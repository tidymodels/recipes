#' Filter rows using dplyr
#'
#' `step_filter` creates a *specification* of a recipe step
#'  that will remove rows using [dplyr::filter()].
#'
#' @inheritParams step_center
#' @param ... Logical predicates defined in terms of the variables
#'  in the data. Multiple conditions are combined with `&`. Only
#'  rows where the condition evaluates to `TRUE` are kept. See
#'  [dplyr::filter()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param inputs Quosure of values given by `...`.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = FALSE`; in most instances that
#'  affect the rows of the data being predicted, this step probably should not
#'  be applied.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  contains the conditional statements. These
#'  expressions are text representations and are not parsable.
#' @details When an object in the user's global environment is
#'  referenced in the expression defining the new variable(s),
#'  it is a good idea to use quasiquotation (e.g. `!!`) to embed
#'  the value of the object in the expression (to be portable
#'  between sessions). See the examples.
#' @keywords datagen
#' @concept preprocessing
#' @concept row_filters
#' @export
#' @examples
#' rec <- recipe( ~ ., data = iris) %>%
#'   step_filter(Sepal.Length > 4.5, Species == "setosa")
#'
#' prepped <- prep(rec, training = iris %>% slice(1:75))
#'
#' library(dplyr)
#'
#' dplyr_train <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(1:75) %>%
#'   dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
#'
#' rec_train <- bake(prepped, new_data = NULL)
#' all.equal(dplyr_train, rec_train)
#'
#' dplyr_test <-
#'   iris %>%
#'   as_tibble() %>%
#'   slice(76:150) %>%
#'   dplyr::filter(Sepal.Length > 4.5, Species != "setosa")
#' rec_test <- bake(prepped, iris %>% slice(76:150))
#' all.equal(dplyr_test, rec_test)
#'
#' values <- c("versicolor", "virginica")
#'
#' qq_rec <-
#'   recipe( ~ ., data = iris) %>%
#'   # Embed the `values` object in the call using !!
#'   step_filter(Sepal.Length > 4.5, Species  %in% !!values)
#'
#' tidy(qq_rec, number = 1)

step_filter <- function(
  recipe, ...,
  role = NA,
  trained = FALSE,
  inputs = NULL,
  skip = TRUE,
  id = rand_id("filter")
) {

  inputs <- enquos(...)

  add_step(
    recipe,
    step_filter_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_filter_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "filter",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_filter <- function(x, training, info = NULL, ...) {
  step_filter_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_filter <- function(object, new_data, ...) {
  dplyr::filter(new_data, !!!object$inputs)
}


print.step_filter <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Row filtering")
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }

#' @rdname step_filter
#' @param x A `step_filter` object
#' @export
tidy.step_filter <- function(x, ...) {
  cond_expr <- map(x$inputs, quo_get_expr)
  cond_expr <- map_chr(cond_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = cond_expr,
    id = rep(x$id, length(x$inputs))
  )
}
