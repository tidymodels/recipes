#' Add intercept (or constant) column
#'
#' `step_intercept()` creates a *specification* of a recipe step that will add
#' an intercept or constant term in the first column of a data matrix.
#' `step_intercept()` defaults to *predictor* role so that it is by default only
#' called in the bake step. Be careful to avoid unintentional transformations
#' when calling steps with `all_predictors()`.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... Argument ignored; included for consistency with other step
#'   specification functions.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated. Again included only for consistency.
#' @param name Character name for newly added column
#' @param value A numeric constant to fill the intercept column. Defaults to
#'   `1L`.
#' @template step-return
#' @export
#'
#' @details
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
#' rec_trans <- recipe(HHV ~ ., data = biomass_tr[, -(1:2)]) |>
#'   step_intercept(value = 2) |>
#'   step_scale(carbon)
#'
#' rec_obj <- prep(rec_trans, training = biomass_tr)
#'
#' with_intercept <- bake(rec_obj, biomass_te)
#' with_intercept
step_intercept <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  name = "intercept",
  value = 1L,
  skip = FALSE,
  id = rand_id("intercept")
) {
  if (length(list(...)) > 0) {
    cli::cli_warn("Selectors are not used for this step.")
  }

  check_number_decimal(value)
  check_string(name)

  add_step(
    recipe,
    step_intercept_new(
      role = role,
      trained = trained,
      name = name,
      value = value,
      skip = skip,
      id = id
    )
  )
}

step_intercept_new <-
  function(role, trained, name, value, skip, id) {
    step(
      subclass = "intercept",
      role = role,
      trained = trained,
      name = name,
      value = value,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_intercept <- function(x, training, info = NULL, ...) {
  x$trained <- TRUE
  x
}

#' @export
bake.step_intercept <- function(object, new_data, ...) {
  intercept <- tibble(!!object$name := rep(object$value, nrow(new_data)))
  intercept <- check_name(intercept, new_data, object, names(intercept))
  new_data <- vec_cbind(intercept, new_data, .name_repair = "minimal")
  new_data
}

#' @export
print.step_intercept <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Adding intercept named: "
    untrained_terms <- rlang::parse_quos(x$name, rlang::current_env())
    print_step(x$name, untrained_terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_intercept <- function(x, ...) {
  res <- tibble(value = x$name)
  res$id <- x$id
  res
}
