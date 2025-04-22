#' Spatial sign preprocessing
#'
#' `step_spatialsign()` is a *specification* of a recipe step that will convert
#' numeric data into a projection on to a unit sphere.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param na_rm A logical: should missing data be removed from the norm
#'   computation?
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#'
#' The spatial sign transformation projects the variables onto a unit sphere and
#' is related to global contrast normalization. The spatial sign of a vector `w`
#' is `w/norm(w)`.
#'
#' The variables should be centered and scaled prior to the computations.
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
#' @section Case weights:
#'
#'   This step performs an unsupervised operation that can utilize case weights.
#'   As a result, only frequency weights are allowed. For more information, see
#'   the documentation in [case_weights] and the examples on `tidymodels.org`.
#'
#'   Unlike most, this step requires the case weights to be available when new
#'   samples are processed (e.g., when `bake()` is used or `predict()` with a
#'   workflow). To tell recipes that the case weights are required at bake time,
#'   use `recipe |> update_role_requirements(role = "case_weights", bake =
#'   TRUE)`. See [update_role_requirements()] for more information.
#'
#' @references Serneels, S., De Nolf, E., and Van Espen, P. (2006). Spatial sign
#'   preprocessing: a simple way to impart moderate robustness to multivariate
#'   estimators. *Journal of Chemical Information and Modeling*, 46(3),
#'   1402-1409.
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
#' ss_trans <- rec |>
#'   step_center(carbon, hydrogen) |>
#'   step_scale(carbon, hydrogen) |>
#'   step_spatialsign(carbon, hydrogen)
#'
#' ss_obj <- prep(ss_trans, training = biomass_tr)
#'
#' transformed_te <- bake(ss_obj, biomass_te)
#'
#' plot(biomass_te$carbon, biomass_te$hydrogen)
#'
#' plot(transformed_te$carbon, transformed_te$hydrogen)
#'
#' tidy(ss_trans, number = 3)
#' tidy(ss_obj, number = 3)
step_spatialsign <-
  function(
    recipe,
    ...,
    role = "predictor",
    na_rm = TRUE,
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("spatialsign")
  ) {
    add_step(
      recipe,
      step_spatialsign_new(
        terms = enquos(...),
        role = role,
        na_rm = na_rm,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_spatialsign_new <-
  function(terms, role, na_rm, trained, columns, skip, id, case_weights) {
    step(
      subclass = "spatialsign",
      terms = terms,
      role = role,
      na_rm = na_rm,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_spatialsign <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$na_rm, arg = "na_rm")

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  step_spatialsign_new(
    terms = x$terms,
    role = x$role,
    na_rm = x$na_rm,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_spatialsign <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  if (isTRUE(object$case_weights)) {
    wts_col <- purrr::map_lgl(new_data, hardhat::is_case_weights)
    wts <- new_data[[names(which(wts_col))]]
    wts <- as.double(wts)
  } else {
    wts <- 1
  }

  res <- as.matrix(new_data[, col_names])
  res <- res / sqrt(rowSums((sqrt(1 / wts) * res)^2, na.rm = object$na_rm))

  res <- tibble::as_tibble(res)
  new_data[, col_names] <- res
  new_data
}

#' @export
print.step_spatialsign <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Spatial sign on  "
    print_step(
      x$columns,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_spatialsign <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
