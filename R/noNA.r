#' Check no NA Values
#'
#' `check_noNA` creates a a *specification* of a recipe
#'  step that will check if variables do not contain missing values.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are checked in the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained Irrelevant for this step since for this check it does not
#'  matter if check is trained or not.
#' @return An updated version of `recipe` with the new check
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @export
#' @details This check will break the `bake` function if any of the checked
#'  columns does contain `NA` values. If the check passes, nothing is changed
#'  to the data.
#' @examples
#' data(credit_data)
#' is.na(credit_data) %>% colSums()
#'
#' # If the test passes, newdata is returned unaltered
#' recipe(credit_data) %>%
#'   check_noNA(Age, Expenses) %>%
#'   prep() %>%
#'   bake(credit_data)
#'
#' # If your training set doesn't pass, prep() will break already
#' recipe(credit_data)  %>%
#'   check_noNA(Income) %>%
#'   prep()
#'
#' # If newdata contain missing values, the check will break bake()
#' train_data <- credit_data %>% dplyr::filter(Income > 150)
#' test_data  <- credit_data %>% dplyr::filter(Income <= 150 | is.na(Income))
#' rp <- recipe(train_data) %>%
#'   check_noNA(Income) %>%
#'   prep()
#' bake(rp, train_data)
#' bake(rp, test_data)
check_noNA <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           na.rm = TRUE,
           columns = NULL) {
    add_step(
      recipe,
      check_noNA_new(
        terms   = check_ellipses(...),
        role    = role,
        trained = trained,
        columns = columns
      )
    )
  }

check_noNA_new <-
  function(terms = NULL,
           role  = NA,
           trained = FALSE,
           columns = NULL) {
    step(subclass = "noNA",
         prefix   = "check_",
         terms    = terms,
         role     = role,
         trained  = trained,
         columns  = columns)
  }

prep.check_noNA <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_noNA_new(terms = x$terms,
                 role  = x$role,
                 trained = TRUE,
                 columns = col_names)
}

bake.check_noNA <- function(object, newdata, ...) {
  col_names       <- object$columns
  subset_to_check <- newdata[col_names]
  nr_na           <- colSums(is.na(subset_to_check))
  if (any(nr_na > 0)) {
    with_na     <- names(nr_na[nr_na > 0])
    with_na_str <- paste(paste0("`", with_na, "`"), collapse = ", ")
    stop(paste0("The following columns contain missing values: ",
                with_na_str,
                "."),
         call. = FALSE)
  }
  newdata
}

print.check_noNA <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Check no NA for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname check_noNA
#' @param x A `check_noNA` object.
## copied this from tidy.step_dummy, candidate for refactoring
tidy.check_noNA <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$levels))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
