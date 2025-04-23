#' Impute numeric data using the median
#'
#' `step_impute_median()` creates a *specification* of a recipe step that will
#' substitute missing values of numeric variables by the training set median of
#' those variables.
#'
#' @inheritParams step_center
#' @param medians A named numeric vector of medians. This is `NULL` until
#'   computed by [prep()]. Note that, if the original data are integers, the
#'   median will be converted to an integer to maintain the same data type.
#' @template step-return
#' @family imputation steps
#' @export
#' @details
#'
#' `step_impute_median()` estimates the variable medians from the data used in
#' the `training` argument of [prep()]. [bake()] then applies the new values to
#' new data sets using these medians.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_medianimpute()`
#' to `step_impute_median()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the median value}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data("credit_data", package = "modeldata")
#'
#' ## missing data per column
#' vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
#'
#' set.seed(342)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[in_training, ]
#' credit_te <- credit_data[-in_training, ]
#' missing_examples <- c(14, 394, 565)
#'
#' rec <- recipe(Price ~ ., data = credit_tr)
#'
#' impute_rec <- rec |>
#'   step_impute_median(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te)
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
step_impute_median <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    medians = NULL,
    skip = FALSE,
    id = rand_id("impute_median")
  ) {
    add_step(
      recipe,
      step_impute_median_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        medians = medians,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_impute_median_new <-
  function(terms, role, trained, medians, skip, id, case_weights) {
    step(
      subclass = "impute_median",
      terms = terms,
      role = role,
      trained = trained,
      medians = medians,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_impute_median <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  medians <- medians(training[, col_names], wts = wts)
  medians <- purrr::map2(medians, training[, col_names], cast)

  step_impute_median_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    medians = medians,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_impute_median <- function(object, new_data, ...) {
  col_names <- names(object$medians)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    median <- object$medians[[col_name]]
    if (sparsevctrs::is_sparse_vector(new_data[[col_name]])) {
      new_data[[col_name]] <- sparsevctrs::sparse_replace_na(
        new_data[[col_name]],
        median
      )
    } else {
      if (anyNA(new_data[[col_name]])) {
        new_data[[col_name]] <- vctrs::vec_cast(new_data[[col_name]], median)
      }
      new_data[is.na(new_data[[col_name]]), col_name] <- median
    }
  }

  new_data
}

#' @export
print.step_impute_median <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Median imputation for "
    print_step(
      names(x$medians),
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_impute_median <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$medians),
      value = vctrs::list_unchop(unname(x$medians), ptype = double())
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl)
  }
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_impute_median <- function(x, ...) {
  TRUE
}
