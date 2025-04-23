#' Check for missing values
#'
#' `check_missing()` creates a *specification* of a recipe operation that will
#' check if variables contain missing values.
#'
#' @inheritParams step_pca
#' @param recipe A recipe object. The check will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables for this check.
#'   See [selections()] for more details.
#' @param role Not used by this check since no new variables are created.
#' @param trained A logical for whether the selectors in `...` have been
#'   resolved by [prep()].
#' @param id A character string that is unique to this check to identify it.
#' @param skip A logical. Should the check be skipped when the recipe is baked
#'   by [bake()]? While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)). Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations.
#' @template check-return
#' @family checks
#' @export
#' @details
#'
#' This check will break the [bake()] function if any of the checked columns
#' does contain `NA` values. If the check passes, nothing is changed to the
#' data.
#'
#' # tidy() results
#'
#' When you [`tidy()`][tidy.recipe()] this check, a tibble with column `terms`
#' (the selectors or variables selected) is returned.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(credit_data, package = "modeldata")
#' is.na(credit_data) |> colSums()
#'
#' # If the test passes, `new_data` is returned unaltered
#' recipe(credit_data) |>
#'   check_missing(Age, Expenses) |>
#'   prep() |>
#'   bake(credit_data)
#'
#' # If your training set doesn't pass, prep() will stop with an error
#' \dontrun{
#' recipe(credit_data) |>
#'   check_missing(Income) |>
#'   prep()
#' }
#'
#' # If `new_data` contain missing values, the check will stop `bake()`
#'
#' train_data <- credit_data |> dplyr::filter(Income > 150)
#' test_data <- credit_data |> dplyr::filter(Income <= 150 | is.na(Income))
#'
#' rp <- recipe(train_data) |>
#'   check_missing(Income) |>
#'   prep()
#'
#' bake(rp, train_data)
#' \dontrun{
#' bake(rp, test_data)
#' }
check_missing <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("missing")
  ) {
    add_check(
      recipe,
      check_missing_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

check_missing_new <-
  function(terms, role, trained, columns, skip, id) {
    check(
      subclass = "missing",
      prefix = "check_",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.check_missing <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.check_missing <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  subset_to_check <- new_data[col_names]
  nr_na <- colSums(is.na(subset_to_check))
  if (any(nr_na > 0)) {
    with_na <- names(nr_na[nr_na > 0])
    cli::cli_abort(
      "The following columns contains missing values: {with_na}."
    )
  }
  new_data
}

#' @export
print.check_missing <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Check missing values for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_missing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
