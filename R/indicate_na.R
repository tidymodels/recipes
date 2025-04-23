#' Create missing data column indicators
#'
#' `step_indicate_na()` creates a *specification* of a recipe step that will
#' create and append additional binary columns to the data set to indicate which
#' observations are missing.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @inheritParams step_dummy
#' @param prefix A character string that will be the prefix to the resulting new
#'   variables. Defaults to "na_ind".
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-creation
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
#' impute_rec <- rec |>
#'   step_indicate_na(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te)
step_indicate_na <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    columns = NULL,
    prefix = "na_ind",
    sparse = "auto",
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("indicate_na")
  ) {
    terms <- enquos(...)

    add_step(
      recipe,
      step_indicate_na_new(
        terms = terms,
        role = role,
        trained = trained,
        columns = columns,
        prefix = prefix,
        sparse = sparse,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_indicate_na_new <-
  function(
    terms,
    role,
    trained,
    columns,
    prefix,
    sparse,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "indicate_na",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      prefix = prefix,
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_indicate_na <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_sparse_arg(x$sparse)

  step_indicate_na_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    prefix = x$prefix,
    sparse = x$sparse,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_indicate_na <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  cols <- list()

  for (col_name in col_names) {
    if (sparse_is_yes(object$sparse)) {
      positions <- which(is.na(new_data[[col_name]]))
      cols[[col_name]] <- sparsevctrs::sparse_integer(
        values = rep(1, length(positions)),
        positions = positions,
        length = length(new_data[[col_name]])
      )
    } else {
      cols[[col_name]] <- ifelse(is.na(new_data[[col_name]]), 1L, 0L)
    }
  }

  cols <- tibble::new_tibble(cols, nrow = nrow(new_data))
  cols <- dplyr::rename_with(cols, ~ vec_paste0(object$prefix, "_", .x))

  cols <- check_name(cols, new_data, object, names(cols))

  new_data <- vec_cbind(new_data, cols, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
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

#' @export
.recipes_estimate_sparsity.step_indicate_na <- function(x, data, ...) {
  lapply(
    seq_len(ncol(data)),
    function(x) c(n_cols = 1, sparsity = 1)
  )
}
