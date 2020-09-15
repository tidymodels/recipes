#' Clean Categorical Levels
#'
#' `step_clean_levels` creates a *specification* of a recipe step that will
#'  clean nominal data so the levels consist only of letters, numbers,
#'  and the underscore.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables that will potentially be reduced. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param clean A named character vector to clean and recode categorical levels.
#'  This is `NULL` until computed by [prep.recipe()]. Note that if the original
#'  variable is a character vector, it will be converted to a factor.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the `tidy` method, a
#'  tibble with columns `terms` (the columns that are cleaned), `original` (the
#'  original uncleaned levels) and `value` (the new cleaned levels).
#' @keywords datagen
#' @concept preprocessing
#' @concept factors
#' @export
#' @details The new levels are cleaned and then reset with
#'   [dplyr::recode_factor()]. When data to be processed contains novel
#'   levels (i.e., not contained in the training set), they are converted
#'   to missing.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()], [step_ordinalscore()],
#'  [step_unorder()], [step_novel()], [step_other()]
#' @examples
#' library(modeldata)
#' data(Smithsonian)
#'
#' smith_tr <- Smithsonian[1:15,]
#' smith_te <- Smithsonian[16:20,]
#'
#' rec <- recipe(~ ., data = smith_tr)
#'
#' rec <- rec %>%
#'   step_clean_levels(name)
#' rec <- prep(rec, training = smith_tr)
#'
#' cleaned <- bake(rec, smith_tr)
#'
#' tidy(rec, number = 1)
#'
#' # novel levels are replaced with missing
#' bake(rec, smith_te)
#'

step_clean_levels <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           clean = NULL,
           skip = FALSE,
           id = rand_id("clean_levels")) {
    add_step(
      recipe,
      step_clean_levels_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        clean = clean,
        skip = skip,
        id = id
      )
    )
  }

step_clean_levels_new <-
  function(terms, role, trained, clean, skip, id) {
    step(
      subclass = "clean_levels",
      terms = terms,
      role = role,
      trained = trained,
      clean = clean,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_clean_levels <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info, empty_fun = passover)
  check_type(training[, col_names], quant = FALSE)

  if (length(col_names) > 0) {
    orig <- purrr::map(training[, col_names], levels)
    cleaned <- purrr::map(orig, janitor::make_clean_names)
    clean <- purrr::map2(cleaned, orig, rlang::set_names)
  } else {
    clean <- NULL
  }

  step_clean_levels_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    clean = clean,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_clean_levels <- function(object, new_data, ...) {
  if (!is.null(object$clean)) {
    for (i in names(object$clean)) {
      new_data[[i]] <- recode_factor(new_data[[i]], !!!object$clean[[i]])
      }
    }

  as_tibble(new_data)
}

print.step_clean_levels <-
  function(x, width = max(20, options()$width - 30), ...) {

    if (x$trained) {
      cleaned <- names(x$clean)
      if (length(cleaned) > 0) {
        cat("Cleaning factor levels for ", sep = "")
        printer(cleaned, x$terms, x$trained, width = width)
      } else {
        cat("No factor levels were cleaned\n")
      }
    } else {
      cat("Cleaning factor levels for ", sep = "")
      printer(names(x$objects), x$terms, x$trained, width = width)
    }
    invisible(x)
  }

#' @rdname step_clean_levels
#' @param x A `step_clean_levels` object.
#' @export
tidy.step_clean_levels <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(x$clean, tibble::enframe,
                          name = "original", .id = "terms")
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}


