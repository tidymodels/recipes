#' Check if all Columns are Present
#'
#' `check_cols` creates a *specification* of a recipe
#'  step that will check if all the columns of the training frame are
#'  present in the newdata.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are checked in the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the column names to check
#'  against are gathered.

## Do we want to keep the rows in cheks_ in the first place. Will
## never be used.

check_cols <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE) {
    add_step(
      recipe,
      check_cols_new(
        terms   = check_ellipses(...),
        role    = role,
        trained = trained,
        columns = NULL
      )
    )
  }

check_cols_new <-
  function(terms = NULL,
           role  = NA,
           trained = FALSE,
           columns = NULL) {
    step(subclass = "cols",
         prefix   = "check_",
         terms    = terms,
         role     = role,
         trained  = trained,
         columns  = columns)
  }

prep.check_cols <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_cols_new(terms = x$terms,
                 role  = x$role,
                 trained = TRUE,
                 columns = col_names)
}

bake.check_cols <- function(object, newdata, ...) {
  # original_cols <- object$columns
  # new_cols      <- names(newdata)
  # missing <- setdiff(original_cols, new_cols)
  # if (length(missing) > 0) {
  #   mis_cols <- paste(paste0("`", missing, "`"), collapse = ", ")
  #   stop("The following cols are missing from newdata: ", mis_cols, ".")
  # }
  "jos"
}

recipe(mtcars) %>% check_cols(everything()) %>% prep(mtcars) %>% bake(mtcars)
