#' Impute Numeric Data Using the Mean
#'
#' `step_meanimpute` creates a *specification* of a recipe step that will
#'  substitute missing values of numeric variables by the training set mean of
#'  those variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param means A named numeric vector of means. This is `NULL` until computed
#'  by [prep.recipe()]. Note that, if the original data are integers, the mean
#'  will be converted to an integer to maintain the same a data type.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#'  end of the variables before the mean is computed. Values of trim outside
#'  that range are taken as the nearest endpoint.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the mean
#'  value).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details `step_meanimpute` estimates the variable means from the data used
#'  in the `training` argument of `prep.recipe`. `bake.recipe` then applies the
#'  new values to new data sets using these averages.
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
#'   step_meanimpute(Income, Assets, Debt)
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

step_meanimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           trim = 0,
           skip = FALSE,
           id = rand_id("meanimpute")) {
    add_step(
      recipe,
      step_meanimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        means = means,
        trim = trim,
        skip = skip,
        id = id
      )
    )
  }

step_meanimpute_new <-
  function(terms, role, trained, means, trim, skip, id) {
    step(
      subclass = "meanimpute",
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
prep.step_meanimpute <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  means <- lapply(training[, col_names], mean, trim = x$trim, na.rm = TRUE)
  means <- purrr::map2(means, training[, col_names], cast)

  step_meanimpute_new(
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
bake.step_meanimpute <- function(object, new_data, ...) {
  for (i in names(object$means)) {
    if (any(is.na(new_data[[i]])))
      new_data[is.na(new_data[[i]]), i] <- object$means[[i]]
  }
  as_tibble(new_data)
}

print.step_meanimpute <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Mean Imputation for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_meanimpute
#' @param x A `step_meanimpute` object.
#' @export
tidy.step_meanimpute <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$means),
                  model = unlist(x$means))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = na_dbl)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.step
#' @export
tunable.step_meanimpute <- function(x, ...) {
  tibble::tibble(
    name = "trim",
    call_info = list(
      list(pkg = "dials", fun = "trim_amount")
    ),
    source = "recipe",
    component = "step_meanimpute",
    component_id = x$id
  )
}

