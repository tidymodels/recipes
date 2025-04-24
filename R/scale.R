#' Scaling numeric data
#'
#' `step_scale()` creates a *specification* of a recipe step that will normalize
#' numeric data to have a standard deviation of one.
#'
#' @inheritParams step_center
#' @param sds A named numeric vector of standard deviations. This is `NULL`
#'   until computed by [prep()].
#' @param factor A numeric value of either 1 or 2 that scales the numeric inputs
#'   by one or two standard deviations. By dividing by two standard deviations,
#'   the coefficients attached to continuous predictors can be interpreted the
#'   same way as with binary inputs. Defaults to `1`. More in reference below.
#' @param na_rm A logical value indicating whether `NA` values should be removed
#'   when computing the standard deviation.
#' @template step-return
#' @family normalization steps
#' @export
#' @details
#'
#' Scaling data means that the standard deviation of a variable is divided out
#' of the data. `step_scale()` estimates the variable standard deviations from
#' the data used in the `training` argument of [prep()]. [bake()] then applies
#' the scaling to new data sets using these standard deviations.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the standard deviations}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-unsupervised
#'
#' @references Gelman, A. (2007) "Scaling regression inputs by
#'  dividing by two standard deviations." Unpublished. Source:
#'  `https://sites.stat.columbia.edu/gelman/research/unpublished/standardizing.pdf`.
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
#' scaled_trans <- rec |>
#'   step_scale(carbon, hydrogen)
#'
#' scaled_obj <- prep(scaled_trans, training = biomass_tr)
#'
#' transformed_te <- bake(scaled_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' tidy(scaled_trans, number = 1)
#' tidy(scaled_obj, number = 1)
step_scale <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    sds = NULL,
    factor = 1,
    na_rm = TRUE,
    skip = FALSE,
    id = rand_id("scale")
  ) {
    add_step(
      recipe,
      step_scale_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        sds = sds,
        factor = factor,
        na_rm = na_rm,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_scale_new <-
  function(terms, role, trained, sds, factor, na_rm, skip, id, case_weights) {
    step(
      subclass = "scale",
      terms = terms,
      role = role,
      trained = trained,
      sds = sds,
      factor = factor,
      na_rm = na_rm,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_scale <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$na_rm, arg = "na_rm")
  if (x$factor != 1 & x$factor != 2) {
    cli::cli_warn(
      "Scaling {.arg factor} should take either a value of 1 or 2, not
       {.obj_type_friendly {x$factor}}."
    )
  }

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  vars <- variances(training[, col_names], wts, na_rm = x$na_rm)
  sds <- sqrt(vars)
  sds <- sd_check(sds)
  sds <- sds * x$factor

  step_scale_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    sds = sds,
    factor = x$factor,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_scale <- function(object, new_data, ...) {
  col_names <- names(object$sds)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    sd <- object$sds[col_name]
    if (sparsevctrs::is_sparse_vector(new_data[[col_name]])) {
      new_data[[col_name]] <- sparsevctrs::sparse_division_scalar(
        new_data[[col_name]],
        sd
      )
    } else {
      new_data[[col_name]] <- new_data[[col_name]] / sd
    }
  }
  new_data
}

#' @export
print.step_scale <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Scaling for "
    print_step(
      names(x$sds),
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
tidy.step_scale <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$sds),
      value = unname(x$sds)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_scale <- function(x, ...) {
  TRUE
}
