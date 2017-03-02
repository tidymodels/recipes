#' Scaling Numeric Data
#'
#' \code{step_scale} creates a \emph{specification} of a recipe step that will normalize numeric data to have a standard deviation of one.
#'
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be scaled.
#' @param role Not used by this step since no new variables are created.
#' @param sds A named numeric vector of standard deviations This is \code{NULL} until computed by \code{\link{learn.recipe}}.
#' @return \code{step_scale}  returns an object of class \code{step_scale}.
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details Scaling data means that the standard deviation of a variable is divided out of the data. \code{step_scale} estimates the variable standard deviations from the data used in the \code{training} argument of \code{learn.recipe}. \code{process.recipe} then applies the scaling to new data sets using these standard deviations.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' library(magrittr)
#' scaled_trans <- rec %>%
#'   step_scale(terms = ~ carbon + hydrogen)
#'
#' scaled_obj <- learn(scaled_trans, training = biomass_tr)
#'
#' transformed_te <- process(scaled_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te

step_scale <- function(recipe, terms, role = NA, trained = FALSE, sds = NULL) {
  add_step(
    recipe,
    step_scale_new(
      terms = terms,
      role = role,
      trained = trained,
      sds = sds
    )
  )
}

step_scale_new <- function(terms = NULL, role = NA, trained = FALSE, sds = NULL) {
  step(
    subclass = "scale",
    terms = terms,
    role = role,
    trained = trained,
    sds = sds
  )
}

#' @importFrom stats sd
learn.step_scale <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info)
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = TRUE)
  step_scale_new(terms = x$terms, role = x$role, trained = TRUE, sds)
}

process.step_scale <- function(object, newdata, ...) {
  res <- sweep(as.matrix(newdata[, names(object$sds)]), 2, object$sds, "/")
  if(is.matrix(res) && ncol(res) == 1) res <- res[,1]
  newdata[, names(object$sds)] <- res
  as_tibble(newdata)
}

print.step_scale <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Scaling for ", sep = "")
  if(x$trained) {
    cat(format_ch_vec(names(x$sd), width = width))
  } else cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
