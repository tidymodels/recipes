#' Inverse logit transformation
#'
#' `step_invlogit()` creates a *specification* of a recipe step that will
#' transform the data from real values to be between zero and one.
#'
#' @inheritParams step_center
#' @inheritParams step_pca
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' The inverse logit transformation takes values on the real line and translates
#' them to be between zero and one using the function `f(x) = 1/(1+exp(-x))`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' ilogit_trans <- rec |>
#'   step_center(carbon, hydrogen) |>
#'   step_scale(carbon, hydrogen) |>
#'   step_invlogit(carbon, hydrogen)
#'
#' ilogit_obj <- prep(ilogit_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ilogit_obj, biomass_te)
#' plot(biomass_te$carbon, transformed_te$carbon)
step_invlogit <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("invlogit")
  ) {
    add_step(
      recipe,
      step_invlogit_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
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
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  step_invlogit_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_invlogit <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  if (nrow(new_data) == 0) {
    return(new_data)
  }

  for (col_name in col_names) {
    new_data[[col_name]] <- binomial()$linkinv(new_data[[col_name]])
  }

  new_data
}

#' @export
print.step_invlogit <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Inverse logit on "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_invlogit <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
