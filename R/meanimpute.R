#' Impute Numeric Data Using the Mean
#'
#' \code{step_meanimpute} creates a \emph{specification} of a recipe step that
#'   will substitute missing values of numeric variables by the training set
#'   mean of those variables.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param means A named numeric vector of means. This is \code{NULL} until
#'   computed by \code{\link{prep.recipe}}.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#'   end of the variables before the mean is computed. Values of trim outside
#'   that range are taken as the nearest endpoint.
#' @keywords datagen
#' @concept preprocessing imputation
#' @export
#' @details \code{step_meanimpute} estimates the variable means from the data
#'   used in the \code{training} argument of \code{prep.recipe}.
#'   \code{bake.recipe} then applies the new values to new data sets using
#'   these averages.
#' @examples
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
#' imputed_te <- bake(imp_models, newdata = credit_te, everything())
#'
#' credit_te[missing_examples,]
#' imputed_te[missing_examples, names(credit_te)]

step_meanimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           trim = 0) {
    add_step(
      recipe,
      step_meanimpute_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        means = means,
        trim = trim
      )
    )
  }

step_meanimpute_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           means = NULL,
           trim = NULL) {
    step(
      subclass = "meanimpute",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      trim = trim
    )
  }

#' @export
prep.step_meanimpute <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (any(info$type[info$variable %in% col_names] != "numeric"))
    stop("All variables for mean imputation should be numeric")
  means <-
    vapply(training[, col_names],
           mean,
           c(mean = 0),
           trim = x$trim,
           na.rm = TRUE)
  step_meanimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means,
    trim = x$trim
  )
}

#' @export
bake.step_meanimpute <- function(object, newdata, ...) {
  for (i in names(object$means)) {
    if (any(is.na(newdata[, i])))
      newdata[is.na(newdata[, i]), i] <- object$means[i]
  }
  as_tibble(newdata)
}

print.step_meanimpute <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Mean Imputation for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }
