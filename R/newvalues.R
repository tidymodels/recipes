#' Check for New Values
#'
#' `check_new_values` creates a a *specification* of a recipe
#'  operation that will check if variables contain new values.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are checked in the check. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param ignore_NA A logical that indicates if we should consider missing
#'  values as value or not. Defaults to `TRUE`.
#' @param values A named list with the allowed values.
#'  This is `NULL` until computed by prep.recipe().
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
#'  columns does contain values it did not contain when `prep` was called
#'  on the recipe. If the check passes, nothing is changed to the data.
#' @examples
#' library(modeldata)
#' data(credit_data)
#'
#' # If the test passes, `new_data` is returned unaltered
#' recipe(credit_data) %>%
#'   check_new_values(Home) %>%
#'   prep() %>%
#'   bake(new_data = credit_data)
#'
#' # If `new_data` contains values not in `x` at the `prep()` function,
#' # the `bake()` function will break.
#' \dontrun{
#' recipe(credit_data %>% dplyr::filter(Home != "rent")) %>%
#'   check_new_values(Home) %>%
#'   prep() %>%
#'   bake(new_data = credit_data)
#' }
#'
#' # By default missing values are ignored, so this passes.
#' recipe(credit_data %>% dplyr::filter(!is.na(Home))) %>%
#'   check_new_values(Home) %>%
#'   prep() %>%
#'   bake(credit_data)
#'
#' # Use `ignore_NA = FALSE` if you consider missing values  as a value,
#' # that should not occur when not observed in the train set.
#' \dontrun{
#' recipe(credit_data %>% dplyr::filter(!is.na(Home))) %>%
#'   check_new_values(Home, ignore_NA = FALSE) %>%
#'   prep() %>%
#'   bake(credit_data)
#' }
check_new_values <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           ignore_NA = TRUE,
           values = NULL,
           skip = FALSE,
           id = rand_id("new_values")) {
    add_check(
      recipe,
      check_new_values_new(
        terms   = ellipse_check(...),
        role    = role,
        trained = trained,
        columns = columns,
        ignore_NA = ignore_NA,
        values = values,
        skip = skip,
        id = id
      )
    )
  }

check_new_values_new <-
  function(terms, role, trained, columns, skip, id, values, ignore_NA) {
    check(subclass  = "new_values",
          prefix    = "check_",
          terms     = terms,
          role      = role,
          trained   = trained,
          columns   = columns,
          skip      = skip,
          id        = id,
          values    = values,
          ignore_NA = ignore_NA)
  }

new_values_func <- function(x,
                            allowed_values,
                            colname = "x",
                            ignore_NA = TRUE) {
  new_vals <- setdiff(as.character(x), as.character(allowed_values))
  if (length(new_vals) == 0) return()
  if (all(is.na(new_vals)) && ignore_NA) return()
  if (ignore_NA) new_vals <- new_vals[!is.na(new_vals)]
  rlang::abort(paste0(
    colname,
    " contains the new value(s): ",
    paste(new_vals, collapse = ",")
  ))
}

prep.check_new_values <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  values <- lapply(training[ ,col_names], unique)
  check_new_values_new(
    terms   = x$terms,
    role    = x$role,
    trained = TRUE,
    columns = col_names,
    skip    = x$skip,
    id      = x$id,
    values  = values,
    ignore_NA = x$ignore_NA
  )
}

bake.check_new_values <- function(object,
                                  new_data,
                                  ...) {
  col_names <- names(object$values)
  for (i in seq_along(col_names)) {
    colname <- col_names[i]
    new_values_func(new_data[[ colname ]],
                    object$values[[colname]],
                    colname,
                    ignore_NA = object$ignore_NA)
  }
  as_tibble(new_data)
}

print.check_new_values <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Checking no new_values for ", sep = "")
    printer(names(x$values), x$terms, x$trained, width = width)
    invisible(x)
  }

tidy.check_new_values <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
