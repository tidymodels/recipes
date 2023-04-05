#' Impute numeric data using the median
#'
#' `step_impute_median` creates a *specification* of a recipe step that will
#'  substitute missing values of numeric variables by the training set median of
#'  those variables.
#'
#' @inheritParams step_center
#' @param medians A named numeric vector of medians. This is `NULL` until
#'  computed by [prep()]. Note that, if the original data are integers,
#'  the median will be converted to an integer to maintain the same data type.
#' @template step-return
#' @family imputation steps
#' @export
#' @details `step_impute_median` estimates the variable medians from the data
#'  used in the `training` argument of `prep.recipe`. `bake.recipe` then applies
#'  the new values to new data sets using these medians.
#'
#'  As of `recipes` 0.1.16, this function name changed from
#'    `step_medianimpute()` to `step_impute_median()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with
#' columns `terms` (the selectors or variables selected) and `model`
#' (themedian value) is returned.
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
#' impute_rec <- rec %>%
#'   step_impute_median(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
step_impute_median <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           medians = NULL,
           skip = FALSE,
           id = rand_id("impute_median")) {
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

#' @rdname step_impute_median
#' @export
step_medianimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           medians = NULL,
           skip = FALSE,
           id = rand_id("impute_median")) {
    lifecycle::deprecate_stop(
      when = "0.1.16",
      what = "recipes::step_medianimpute()",
      with = "recipes::step_impute_median()"
    )
    step_impute_median(
      recipe,
      ...,
      role = role,
      trained = trained,
      medians = medians,
      skip = skip,
      id = id
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
#' @keywords internal
prep.step_medianimpute <- prep.step_impute_median

#' @export
bake.step_impute_median <- function(object, new_data, ...) {
  check_new_data(names(object$medians), object, new_data)

  for (i in names(object$medians)) {
    if (any(is.na(new_data[[i]]))) {
      new_data[[i]] <- vec_cast(new_data[[i]], object$medians[[i]])
    }
    new_data[is.na(new_data[[i]]), i] <- object$medians[[i]]
  }
  new_data
}

#' @export
#' @keywords internal
bake.step_medianimpute <- bake.step_impute_median

#' @export
print.step_impute_median <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Median imputation for "
    print_step(names(x$medians), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }

#' @export
#' @keywords internal
print.step_medianimpute <- print.step_impute_median

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
#' @keywords internal
tidy.step_medianimpute <- tidy.step_impute_median
