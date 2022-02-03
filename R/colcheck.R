#' Check if all Columns are Present
#'
#' `check_cols` creates a *specification* of a recipe
#'  step that will check if all the columns of the training frame are
#'  present in the new data.
#'
#' @inheritParams check_missing
#' @template check-return
#' @family checks
#' @export
#' @details This check will break the `bake` function if any of the specified
#' columns is not present in the data. If the check passes, nothing is changed
#'  to the data.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this check, a tibble with columns
#'  `terms` (the selectors or variables selected) and `value` (the type)
#'  is returned.
#' @examples
#'
#' library(modeldata)
#' data(biomass)
#'
#' biomass_rec <- recipe(HHV ~ ., data = biomass) %>%
#'    step_rm(sample, dataset) %>%
#'    check_cols(contains("gen")) %>%
#'    step_center(all_numeric_predictors())
#'
#' \dontrun{
#' bake(biomass_rec, biomass[, c("carbon", "HHV")])
#' }
check_cols <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           skip = FALSE,
           id = rand_id("cols")) {
    add_check(
      recipe,
      check_cols_new(
        terms   = enquos(...),
        role    = role,
        trained = trained,
        columns = NULL,
        skip = skip,
        id = id
      )
    )
  }

check_cols_new <-
  function(terms, role, trained, columns, skip, id) {
    check(subclass = "cols",
          prefix   = "check_",
          terms    = terms,
          role     = role,
          trained  = trained,
          columns  = columns,
          skip     = skip,
          id       = id)
  }

prep.check_cols <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_cols_new(
    terms = x$terms,
    role  = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

bake.check_cols <- function(object, new_data, ...) {
  original_cols <- object$columns
  new_cols      <- names(new_data)
  missing <- setdiff(original_cols, new_cols)
  if (length(missing) > 0) {
    mis_cols <- paste(paste0("`", missing, "`"), collapse = ", ")
    rlang::abort(
      paste0(
        "The following cols are missing from `new_data`: ",
         mis_cols,
        "."
        )
    )
  }
  new_data
}

print.check_cols <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Check if the following columns are present: "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_cols <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}

