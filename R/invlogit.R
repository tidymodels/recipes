#' Inverse Logit Transformation
#'
#' `step_invlogit` creates a *specification* of a recipe
#'  step that will transform the data from real values to be between
#'  zero and one.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @details The inverse logit transformation takes values on the
#'  real line and translates them to be between zero and one using
#'  the function `f(x) = 1/(1+exp(-x))`.
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
#' ilogit_obj <- prep(ilogit_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ilogit_obj, biomass_te)
#' plot(biomass_te$carbon, transformed_te$carbon)
#' @seealso [step_logit()] [step_log()]
#'   [step_sqrt()]  [step_hyperbolic()]
#'   [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_invlogit <-
  function(recipe, ...,  role = NA, trained = FALSE, columns = NULL,
           skip = FALSE, id = rand_id("invlogit")) {
    add_step(recipe,
             step_invlogit_new(
               terms = ellipse_check(...),
               role = role,
               trained = trained,
               columns = columns,
               skip = skip,
               id = id
             ))
  }

step_invlogit_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "invlogit",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_invlogit <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  step_invlogit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tibble as_tibble
#' @importFrom stats binomial
#' @export
bake.step_invlogit <- function(object, new_data, ...) {
  for (i in seq_along(object$columns))
    new_data[, object$columns[i]] <-
      binomial()$linkinv(unlist(getElement(new_data, object$columns[i]),
                                use.names = FALSE))
  as_tibble(new_data)
}


print.step_invlogit <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Inverse logit on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_invlogit
#' @param x A `step_invlogit` object.
#' @export
tidy.step_invlogit <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
