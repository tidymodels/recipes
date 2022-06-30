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
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected) and `model` (the
#' median value) is returned.
#'
#' @template case-weights-not-supported
#'
#' @family dummy variable and encoding steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' data("credit_data", package = "modeldata")
#'
#' ## missing data per column
#' purrr::map_dbl(credit_data, function(x) mean(is.na(x)))
#'
#' set.seed(342)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[in_training, ]
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
    terms <- enquos(...)

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
  col_names <- recipes_eval_select(x$terms, training, info)

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
  check_new_data(names(object$columns), object, new_data)

  col_names <- object$columns

  cols <- purrr::map(
    new_data[col_names],
    ~ ifelse(is.na(.x), 1L, 0L)
  )

  cols <- tibble::new_tibble(cols, nrow = nrow(new_data))
  cols <- dplyr::rename_with(cols, ~ paste0(object$prefix, "_", .x))

  new_data <- dplyr::bind_cols(new_data, cols)
  new_data
}

print.step_indicate_na <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Creating missing data variable indicators for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_indicate_na <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = unname(x$columns))
  } else {
    res <- tibble::tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
