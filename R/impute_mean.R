#' Impute numeric data using the mean
#'
#' `step_impute_mean()` creates a *specification* of a recipe step that will
#' substitute missing values of numeric variables by the training set mean of
#' those variables.
#'
#' @inheritParams step_center
#' @param means A named numeric vector of means. This is `NULL` until computed
#'   by [prep()]. Note that, if the original data are integers, the mean will be
#'   converted to an integer to maintain the same data type.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#'   end of the variables before the mean is computed. Values of trim outside
#'   that range are taken as the nearest endpoint.
#' @template step-return
#' @family imputation steps
#' @export
#' @details
#'
#' `step_impute_mean()` estimates the variable means from the data used in the
#' `training` argument of [prep()]. [bake()] then applies the new values to new
#' data sets using these averages.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_meanimpute()`
#' to `step_impute_mean()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the mean value}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_impute_mean"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
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
#'   step_impute_mean(Income, Assets, Debt)
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
step_impute_mean <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    means = NULL,
    trim = 0,
    skip = FALSE,
    id = rand_id("impute_mean")
  ) {
    add_step(
      recipe,
      step_impute_mean_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        means = means,
        trim = trim,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_impute_mean_new <-
  function(terms, role, trained, means, trim, skip, id, case_weights) {
    step(
      subclass = "impute_mean",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      trim = trim,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

trim <- function(x, trim) {
  if (trim == 0) {
    return(x)
  }
  # Adapted from mean.default
  x <- sort(x, na.last = TRUE)
  na_ind <- is.na(x)
  n <- length(x[!na_ind])
  if (trim > 0 && n) {
    if (trim >= 0.5) return(stats::median(x[!na_ind], na.rm = FALSE))
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x[seq(1, lo - 1)] <- NA
    x[seq(hi + 1, n)] <- NA
  }
  x
}

#' @export
prep.step_impute_mean <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$trim, arg = "trim", min = 0, max = 1 / 2)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  trimmed <- purrr::map(training[, col_names], trim, x$trim)
  trimmed <- vctrs::vec_cbind(!!!trimmed)

  means <- averages(trimmed, wts = wts)
  means <- purrr::map2(means, trimmed, cast)

  step_impute_mean_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means,
    trim = x$trim,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_impute_mean <- function(object, new_data, ...) {
  col_names <- names(object$means)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    mean <- object$means[[col_name]]
    if (sparsevctrs::is_sparse_vector(new_data[[col_name]])) {
      new_data[[col_name]] <- sparsevctrs::sparse_replace_na(
        new_data[[col_name]],
        mean
      )
    } else {
      if (anyNA(new_data[[col_name]])) {
        new_data[[col_name]] <- vctrs::vec_cast(new_data[[col_name]], mean)
      }
      new_data[is.na(new_data[[col_name]]), col_name] <- mean
    }
  }

  new_data
}

#' @export
print.step_impute_mean <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Mean imputation for "
    print_step(
      names(x$means),
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
tidy.step_impute_mean <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$means),
      value = vctrs::list_unchop(unname(x$means), ptype = double())
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_impute_mean <- function(x, ...) {
  tibble::tibble(
    name = "trim",
    call_info = list(
      list(pkg = "dials", fun = "trim_amount")
    ),
    source = "recipe",
    component = "step_impute_mean",
    component_id = x$id
  )
}

#' @export
.recipes_preserve_sparsity.step_impute_mean <- function(x, ...) {
  TRUE
}
