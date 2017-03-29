#' Inverse Logit Transformation
#'
#' \code{step_invlogit} creates a \emph{specification} of a recipe step that will transform the data from real values to be between zero and one.
#'
#' @inheritParams step_center
#' @param role Not used by this step since no new variables are created.
#' @param vars A character string of variable names that will be (eventually) populated by the \code{terms} argument.
#' @return \code{step_invlogit} returns an object of class \code{step_invlogit}.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @details The inverse logit transformation takes values on the real line and translates them to be between zero and one using the function \code{f(x) = 1/(1+exp(-x))}.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' ilogit_trans <- rec  %>%
#'   step_center(carbon, hydrogen) %>%
#'   step_scale(carbon, hydrogen) %>%
#'   step_invlogit(carbon, hydrogen)
#'
#' ilogit_obj <- learn(ilogit_trans, training = biomass_tr)
#'
#' transformed_te <- process(ilogit_obj, biomass_te)
#' plot(biomass_te$carbon, transformed_te$carbon)
#' @seealso \code{\link{step_logit}} \code{\link{step_log}}  \code{\link{step_sqrt}}  \code{\link{step_hyperbolic}}  \code{\link{recipe}} \code{\link{learn.recipe}} \code{\link{process.recipe}}

step_invlogit <- function(recipe, ..., role = NA, trained = FALSE, vars = NULL) {
  terms <- dots_quos(...)
  if(is_empty(terms))
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(
    recipe,
    step_invlogit_new(
      terms = terms,
      role = role,
      trained = trained,
      vars = vars
    )
  )
}

step_invlogit_new <- function(terms = NULL, role = NA, trained = FALSE, vars = NULL) {
  step(
    subclass = "invlogit",
    terms = terms,
    role = role,
    trained = trained,
    vars = vars
  )
}

#' @export
learn.step_invlogit <- function(x, training, info = NULL, ...) {
  col_names <- select_terms(x$terms, info = info)
  step_invlogit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    vars = col_names
  )
}

#' @importFrom tibble as_tibble
#' @importFrom stats binomial
#' @export
process.step_invlogit <- function(object, newdata, ...) {
  for(i in seq_along(object$vars))
    newdata[ , object$vars[i] ] <-
      binomial()$linkinv(unlist(newdata[, object$vars[i] ], use.names = FALSE))
  as_tibble(newdata)
}


print.step_invlogit <- function(x, width = max(20, options()$width - 26), ...) {
  cat("Inverse logit on ", sep = "")
  if(x$trained) {
    cat(format_ch_vec(x$vars, width = width))
  } else cat(format_selectors(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}

