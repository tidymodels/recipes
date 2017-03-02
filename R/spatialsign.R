#' Spatial Sign Preprocessing
#' 
#' \code{step_spatialsign} is a \emph{specification} of a recipe step that will convert numeric data into a projection on to a unit sphere. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used for the normalization.
#' @param role For model terms created by this step, what analysis role should they be assigned? 
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_spatialsign} returns an object of class \code{step_spatialsign}. 
#' @keywords datagen
#' @concept preprocessing projection_methods
#' @export
#' @details The spatial sign transformation projects the variables onto a unit sphere and is related to global contrast normalization. The spatial sign of a vector \code{w} is \code{w/norm(w)}.
#' 
#' The variables should be centered and scaled prior to the computations. 
#' @references Serneels, S., De Nolf, E., and Van Espen, P. (2006). Spatial sign preprocessing: a simple way to impart moderate robustness to multivariate estimators. \emph{Journal of Chemical Information and Modeling}, 46(3), 1402-1409.
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
#' ss_trans <- rec %>%
#'   step_center(terms = ~ carbon + hydrogen) %>%
#'   step_scale(terms = ~ carbon + hydrogen) %>%
#'   step_spatialsign(terms = ~ carbon + hydrogen) 
#' 
#' ss_obj <- learn(ss_trans, training = biomass_tr)
#' 
#' transformed_te <- process(ss_obj, biomass_te)
#' 
#' plot(biomass_te$carbon, biomass_te$hydrogen)
#' 
#' plot(transformed_te$carbon, transformed_te$hydrogen)




step_spatialsign <- function(recipe, 
                             terms, 
                             role = "predictor",
                             trained = FALSE, 
                             vars = NULL) {
  add_step(
    recipe, 
    step_spatialsign_new(
      terms = terms, 
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_spatialsign_new <- function(terms = NULL, 
                                 role = "predictor",
                                 trained = FALSE, 
                                 vars = NULL) {
  
  step(
    subclass = "spatialsign",
    terms = terms,
    role = role,
    trained = trained,
    vars = vars
  )
}

learn.step_spatialsign <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

process.step_spatialsign <- function(object, newdata, ...) {
  col_names <- object$vars
  ss <- function(x) x/sqrt(sum(x^2))
  newdata[, col_names] <- t(apply(as.matrix(newdata[, col_names]), 1, ss))
  as_tibble(newdata)
}

print.step_spatialsign <- function(x, width = 30, ...) {
  cat("Spatial sign on ")
  cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
