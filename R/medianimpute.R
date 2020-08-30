#' Impute Numeric Data Using the Median
#'
#' `step_medianimpute` creates a *specification* of a recipe step that will
#'  substitute missing values of numeric variables by the training set median of
#'  those variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param medians A named numeric vector of medians. This is `NULL` until
#'  computed by [prep.recipe()]. Note that, if the original data are integers,
#'  the median will be converted to an integer to maintain the same data type.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  median value).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details `step_medianimpute` estimates the variable medians from the data
#'  used in the `training` argument of `prep.recipe`. `bake.recipe` then applies
#'  the new values to new data sets using these medians.
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
#'   step_medianimpute(Income, Assets, Debt)
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

step_medianimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           medians = NULL,
           skip = FALSE,
           id = rand_id("medianimpute")) {
    add_step(
      recipe,
      step_medianimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        medians = medians,
        skip = skip,
        id = id
      )
    )
  }

step_medianimpute_new <-
  function(terms, role, trained, medians, skip, id) {
    step(
      subclass = "medianimpute",
      terms = terms,
      role = role,
      trained = trained,
      medians = medians,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_medianimpute <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  medians <- lapply(training[, col_names], median, na.rm = TRUE)
  medians <- purrr::map2(medians, training[, col_names], cast)

  step_medianimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    medians = medians,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_medianimpute <- function(object, new_data, ...) {
  for (i in names(object$medians)) {
    if (any(is.na(new_data[[i]])))
      new_data[is.na(new_data[[i]]), i] <- object$medians[[i]]
  }
  as_tibble(new_data)
}

print.step_medianimpute <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Median Imputation for ", sep = "")
    printer(names(x$medians), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_medianimpute
#' @param x A `step_medianimpute` object.
#' @export
tidy.step_medianimpute <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$medians),
                  model = unlist(x$medians))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = na_dbl)
  }
  res$id <- x$id
  res
}
