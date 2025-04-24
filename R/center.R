#' Centering numeric data
#'
#' `step_center()` creates a *specification* of a recipe step that will
#' normalize numeric data to have a mean of zero.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param means A named numeric vector of means. This is `NULL` until computed
#'   by [prep()].
#' @param na_rm A logical value indicating whether `NA` values should be removed
#'   during computations.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [bake()]? While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)). Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
#' @family normalization steps
#' @export
#' @details
#'
#' Centering data means that the average of a variable is subtracted from the
#' data. `step_center()` estimates the variable means from the data used in the
#' `training` argument of [prep()]. [bake()] then applies the centering to new
#' data sets using these means.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the means}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-unsupervised
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
#' center_trans <- rec |>
#'   step_center(carbon, contains("gen"), -hydrogen)
#'
#' center_obj <- prep(center_trans, training = biomass_tr)
#'
#' transformed_te <- bake(center_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#'
#' tidy(center_trans, number = 1)
#' tidy(center_obj, number = 1)
step_center <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    means = NULL,
    na_rm = TRUE,
    skip = FALSE,
    id = rand_id("center")
  ) {
    add_step(
      recipe,
      step_center_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        means = means,
        na_rm = na_rm,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

## Initializes a new object
step_center_new <-
  function(terms, role, trained, means, na_rm, skip, id, case_weights) {
    step(
      subclass = "center",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      na_rm = na_rm,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_center <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  means <- averages(training[, col_names], wts, na_rm = x$na_rm)

  inf_cols <- col_names[is.infinite(means)]
  if (length(inf_cols) > 0) {
    cli::cli_warn(
      "Column{?s} {.var {inf_cols}} returned NaN. \\
      Consider avoiding `Inf` values before normalising."
    )
  }

  step_center_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_center <- function(object, new_data, ...) {
  col_names <- names(object$means)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    mean <- object$means[col_name]
    new_data[[col_name]] <- new_data[[col_name]] - mean
  }

  new_data
}

#' @export
print.step_center <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Centering for "
    print_step(
      names(x$means),
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
tidy.step_center <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$means),
      value = unname(x$means)
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
