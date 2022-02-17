#' Impute numeric data using the mean
#'
#' `step_impute_mean` creates a *specification* of a recipe step that will
#'  substitute missing values of numeric variables by the training set mean of
#'  those variables.
#'
#' @inheritParams step_center
#' @param means A named numeric vector of means. This is `NULL` until computed
#'  by [prep()]. Note that, if the original data are integers, the mean
#'  will be converted to an integer to maintain the same data type.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#'  end of the variables before the mean is computed. Values of trim outside
#'  that range are taken as the nearest endpoint.
#' @template step-return
#' @family imputation steps
#' @export
#' @details `step_impute_mean` estimates the variable means from the data used
#'  in the `training` argument of `prep.recipe`. `bake.recipe` then applies the
#'  new values to new data sets using these averages.
#'
#'  As of `recipes` 0.1.16, this function name changed from `step_meanimpute()`
#'    to `step_impute_mean()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected) and `model` (the mean
#' value) is returned.
#'
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
#' missing_examples <- c(14, 394, 565)
#'
#' rec <- recipe(Price ~ ., data = credit_tr)
#'
#' impute_rec <- rec %>%
#'   step_impute_mean(Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples,]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)

step_impute_mean <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           trim = 0,
           skip = FALSE,
           id = rand_id("impute_mean")) {
    add_step(
      recipe,
      step_impute_mean_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        means = means,
        trim = trim,
        skip = skip,
        id = id
      )
    )
  }

#' @rdname step_impute_mean
#' @export
step_meanimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           trim = 0,
           skip = FALSE,
           id = rand_id("impute_mean")) {
    lifecycle::deprecate_warn(
      when = "0.1.16",
      what = "recipes::step_meanimpute()",
      with = "recipes::step_impute_mean()"
    )
    step_impute_mean(
      recipe,
      ...,
      role = role,
      trained = trained,
      means = means,
      trim = trim,
      skip = skip,
      id = id
    )
  }

step_impute_mean_new <-
  function(terms, role, trained, means, trim, skip, id) {
    step(
      subclass = "impute_mean",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      trim = trim,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_impute_mean <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  means <- lapply(training[, col_names], mean, trim = x$trim, na.rm = TRUE)
  means <- purrr::map2(means, training[, col_names], cast)

  step_impute_mean_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means,
    trim = x$trim,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @keywords internal
prep.step_meanimpute <- prep.step_impute_mean

#' @export
bake.step_impute_mean <- function(object, new_data, ...) {
  for (i in names(object$means)) {
    if (any(is.na(new_data[[i]])))
      new_data[[i]] <- vec_cast(new_data[[i]], object$means[[i]])
      new_data[is.na(new_data[[i]]), i] <- object$means[[i]]
  }
  as_tibble(new_data)
}

#' @export
#' @keywords internal
bake.step_meanimpute <- bake.step_impute_mean

#' @export
print.step_impute_mean <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Mean imputation for "
    print_step(names(x$means), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @export
#' @keywords internal
print.step_meanimpute <- print.step_impute_mean

#' @rdname tidy.recipe
#' @export
tidy.step_impute_mean <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$means),
                  model = vctrs::vec_unchop(unname(x$means), ptype = double()))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = na_dbl)
  }
  res$id <- x$id
  res
}

#' @export
#' @keywords internal
tidy.step_meanimpute <- tidy.step_impute_mean

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
#' @keywords internal
tunable.step_meanimpute <- tunable.step_impute_mean
