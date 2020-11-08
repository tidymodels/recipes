#' Check for Missing Values
#'
#' `check_missing` creates a *specification* of a recipe
#'  operation that will check if variables contain missing values.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are checked in the check See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the check be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @return An updated version of `recipe` with the new check
#'  added to the sequence of existing operations (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @export
#' @details This check will break the `bake` function if any of the checked
#'  columns does contain `NA` values. If the check passes, nothing is changed
#'  to the data.
#' @examples
#' library(modeldata)
#' data(credit_data)
#' is.na(credit_data) %>% colSums()
#'
#' # If the test passes, `new_data` is returned unaltered
#' recipe(credit_data) %>%
#'   check_missing(Age, Expenses) %>%
#'   prep() %>%
#'   bake(credit_data)
#'
#' # If your training set doesn't pass, prep() will stop with an error
#'
#' \dontrun{
#' recipe(credit_data)  %>%
#'   check_missing(Income) %>%
#'   prep()
#' }
#'
#' # If `new_data` contain missing values, the check will stop bake()
#'
#' train_data <- credit_data %>% dplyr::filter(Income > 150)
#' test_data  <- credit_data %>% dplyr::filter(Income <= 150 | is.na(Income))
#'
#' rp <- recipe(train_data) %>%
#'   check_missing(Income) %>%
#'   prep()
#'
#' bake(rp, train_data)
#' \dontrun{
#' bake(rp, test_data)
#' }
check_missing <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("missing")) {
    add_check(
      recipe,
      check_missing_new(
        terms   = ellipse_check(...),
        role    = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

check_missing_new <-
  function(terms, role, trained, columns, skip, id) {
    check(subclass = "missing",
          prefix   = "check_",
          terms    = terms,
          role     = role,
          trained  = trained,
          columns  = columns,
          skip     = skip,
          id       = id)
  }

prep.check_missing <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_missing_new(terms = x$terms,
                    role  = x$role,
                    trained = TRUE,
                    columns = col_names,
                    skip = x$skip,
                    id = x$id)
}

bake.check_missing <- function(object, new_data, ...) {
  col_names       <- object$columns
  subset_to_check <- new_data[col_names]
  nr_na           <- colSums(is.na(subset_to_check))
  if (any(nr_na > 0)) {
    with_na     <- names(nr_na[nr_na > 0])
    with_na_str <- paste(paste0("`", with_na, "`"), collapse = ", ")
    rlang::abort(paste0("The following columns contain missing values: ",
                        with_na_str, "."))
  }
  new_data
}

print.check_missing <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Check missing values for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname check_missing
#' @param x A `check_missing` object.
#' @export
tidy.check_missing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
