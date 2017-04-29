#' Impute Nominal Data Using the Most Common Value
#'
#' \code{step_modeimpute} creates a \emph{specification} of a recipe step that will substitute missing values of nominal variables by the training set mode of those variables.
#'
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created.
#' @param modes A named character vector of modes. This is \code{NULL} until computed by \code{\link{learn.recipe}}.
#' @return \code{step_modeimpute}  returns an object of class \code{step_modeimpute}.
#' @keywords datagen
#' @concept preprocessing imputation
#' @export
#' @details \code{step_modeimpute} estimates the variable modes from the data used in the \code{training} argument of \code{learn.recipe}. \code{process.recipe} then applies the new values to new data sets using these values. If the training set data has more than one mode, one is selected at random.
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
#'   step_modeimpute(Status, Home, Marital)
#' 
#' imp_models <- learn(impute_rec, training = credit_tr)
#' 
#' imputed_te <- process(imp_models, newdata = credit_te, everything())
#' 
#' table(credit_te$Home, imputed_te$Home, useNA = "always")

step_modeimpute <- function(recipe, ..., role = NA, trained = FALSE, modes = NULL) {
  terms <- quos(...)
  if(is_empty(terms))
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(
    recipe,
    step_modeimpute_new(
      terms = terms,
      role = role,
      trained = trained,
      modes = modes
    )
  )
}

step_modeimpute_new <- function(terms = NULL, role = NA, trained = FALSE, modes = NULL) {
  step(
    subclass = "modeimpute",
    terms = terms,
    role = role,
    trained = trained,
    modes = modes
  )
}

#' @export
learn.step_modeimpute <- function(x, training, info = NULL, ...) {
  col_names <- select_terms(x$terms, info = info)
  modes <- vapply(training[, col_names], mode_est, c(mode = ""))
  step_modeimpute_new(terms = x$terms, role = x$role, trained = TRUE, modes)
}

#' @export
process.step_modeimpute <- function(object, newdata, ...) {
  for(i in names(object$modes)) {
    if(any(is.na(newdata[, i])))
      newdata[is.na(newdata[, i]), i] <- object$modes[i]
  }
  
  as_tibble(newdata)
}

print.step_modeimpute <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Mode Imputation for ", sep = "")
  if(x$trained) {
    cat(format_ch_vec(names(x$modes), width = width))
  } else cat(format_selectors(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

mode_est <- function(x) {
  if(!is.character(x) & !is.factor(x))
    stop("The data should be character or factor to compute the mode.", call. = FALSE)
  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  sample(modes, size = 1)
}


