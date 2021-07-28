#' Create Missing Data Column Indicators
#'
#' `step_indicate_na` creates a *specification* of a recipe step that will
#'  create and append additional binary columns to the dataset to indicate
#'  which observations are missing.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the terms argument.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. Defaults to "na_ind".
#' @template step-return
#' @details  When you [`tidy()`] this step, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  median value) is returned.
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

#' @rdname tidy.recipe
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
