#' Create Missing Data Column Indicators
#'
#' `step_indicate_na` creates a *specification* of a recipe step that will
#'  create and append additional binary columns to the dataset to indicate
#'  which observations are missing.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
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
#'  resulting new variables. Defaults to "na_ind".
#' @param skip A logical. Should the check be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
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
#' purrr::map_dbl(credit_data, function(x) mean(is.na(x)))
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
#'   step_indicate_na(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())

step_indicate_na <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           prefix = "na_ind",
           skip = FALSE,
           id = rand_id("indicate_na")) {

    terms = ellipse_check(...)

    add_step(
      recipe,
      step_indicate_na_new(
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

step_indicate_na_new <-
  function(terms, role, trained, columns, prefix, skip, id) {
    step(
      subclass = "indicate_na",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_indicate_na <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info)

  step_indicate_na_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_indicate_na <- function(object, new_data, ...) {
  col_names <- object$columns

  df_ind_na <- purrr::map_dfc(
      new_data[col_names],
      ~ifelse(is.na(.x), 1L, 0L)
    ) %>%
    dplyr::rename_with(~paste0(object$prefix, "_", .x))
  new_data <- dplyr::bind_cols(new_data, df_ind_na)

  tibble::as_tibble(new_data)
}

print.step_indicate_na <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Creating missing data variable indicators for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_indicate_na
#' @param x A `step_indicate_na` object.
#' @export
tidy.step_indicate_na <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = x$columns)
  } else {
    res <- tibble::tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}

