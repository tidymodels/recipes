#' Create Missing Data Column Indicators
#'
#' `step_na_indicate` creates a *specification* of a recipe step that will
#'  create and append additional binary columns to the dataset, to indicate
#'  which observations are missing.
#'
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new na indicator columns created from the original
#'  variables will be used as predictors in a model.
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  median value).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @examples
#' library(modeldata)
#' data("credit_data")
#'
#' ## missing data per column
#' vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
#'
#' set.seed(342)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[ in_training, ]
#' credit_te <- credit_data[-in_training, ]
#'
#' rec <- recipe(Price ~ ., data = credit_tr)
#'
#' impute_rec <- rec %>%
#'   step_na_indicate(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())

step_na_indicate <-
  function(recipe,
           ...,
           role    = "predictor",
           trained = FALSE,
           columns = NULL,
           prefix  = NULL,
           skip    = FALSE,
           id = rand_id("na_indicate")) {

    terms = ellipse_check(...)

    add_step(
      recipe,
      step_na_indicate_new(
        terms = terms,
        role = role,
        trained = trained,
        columns = columns,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_na_indicate_new <-
  function(terms, role, trained, columns, prefix, skip, id) {
    step(
      subclass = "na_indicate",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      prefix = "na_ind",
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_na_indicate <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  # TODO add other checks
  # check_type(training[, col_names])

  step_na_indicate_new(
    terms   = x$terms,
    role    = x$role,
    trained = TRUE,
    columns = col_names,
    prefix  = x$prefix,
    skip    = x$skip,
    id      = x$id
  )
}

#' @export
bake.step_na_indicate <- function(object, new_data, ...) {
  col_names <- object$columns

  for (i in seq_along(col_names)) {
    col <- new_data[[col_names[i]]]
    # NOTE the original column should remain
    new_data[, col_names[i]] <- col
    # NOTE adding indicator columns with a prefix
    new_data[, paste0(object$prefix, "_", col_names[i])] <- ifelse(is.na(col), 1, 0)
  }
  as_tibble(new_data)
}

print.step_na_indicate <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Creating missing data variable indicators for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_na_indicate
#' @param x A `step_na_indicate` object.
#' @export
tidy.step_shadow_missing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}

