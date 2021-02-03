#' Impute Nominal Data Using the Most Common Value
#'
#'   `step_impute_mode` creates a *specification* of a
#'  recipe step that will substitute missing values of nominal
#'  variables by the training set mode of those variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param modes A named character vector of modes. This is
#'  `NULL` until computed by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `model` (the mode
#'  value).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details `step_impute_mode` estimates the variable modes
#'  from the data used in the `training` argument of
#'  `prep.recipe`. `bake.recipe` then applies the new
#'  values to new data sets using these values. If the training set
#'  data has more than one mode, one is selected at random.
#'
#'  As of `recipes` 0.1.16, this function name changed from `step_modeimpute()`
#'    to `step_impute_mode()`.
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
#'   step_impute_mode(Status, Home, Marital)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' table(credit_te$Home, imputed_te$Home, useNA = "always")
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)

step_impute_mode <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           modes = NULL,
           skip = FALSE,
           id = rand_id("impute_mode")) {
    add_step(
      recipe,
      step_impute_mode_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        modes = modes,
        skip = skip,
        id = id
      )
    )
  }

#' @rdname step_impute_mode
#' @export
step_modeimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           modes = NULL,
           skip = FALSE,
           id = rand_id("impute_mode")) {
    lifecycle::deprecate_soft(
      when = "0.1.16",
      what = "recipes::step_modeimpute()",
      with = "recipes::step_impute_mode()"
    )
    step_impute_mode(
      recipe,
      ...,
      role = role,
      trained = trained,
      modes = modes,
      skip = skip,
      id = id
    )
  }

step_impute_mode_new <-
  function(terms, role, trained, modes, skip, id) {
    step(
      subclass = "impute_mode",
      terms = terms,
      role = role,
      trained = trained,
      modes = modes,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_impute_mode <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)
  modes <- vapply(training[, col_names], mode_est, c(mode = ""))
  step_impute_mode_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    modes = modes,
    skip = x$skip,
    id = x$id
  )
}

#' @export
prep.step_modeimpute <- prep.step_impute_mode

#' @export
bake.step_impute_mode <- function(object, new_data, ...) {
  for (i in names(object$modes)) {
    if (any(is.na(new_data[, i]))) {
      mode_val <- cast(object$modes[[i]], new_data[[i]])
      new_data[is.na(new_data[[i]]), i] <- mode_val
    }
  }
  as_tibble(new_data)
}

#' @export
bake.step_modeimpute <- bake.step_impute_mode

#' @export
print.step_impute_mode <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Mode Imputation for ", sep = "")
    printer(names(x$modes), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
print.step_modeimpute <- print.step_impute_mode

mode_est <- function(x) {
  if (!is.character(x) & !is.factor(x))
    rlang::abort("The data should be character or factor to compute the mode.")
  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  sample(modes, size = 1)
}

#' @rdname step_impute_mode
#' @param x A `step_impute_mode` object.
#' @export
tidy.step_impute_mode <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$modes),
                  model = x$modes)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = na_chr)
  }
  res$id <- x$id
  res
}

#' @export
tidy.step_modeimpute <- tidy.step_impute_mode
