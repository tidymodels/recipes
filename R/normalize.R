#' Center and scale numeric data
#'
#' `step_normalize()` creates a *specification* of a recipe step that will
#' normalize numeric data to have a standard deviation of one and a mean of
#' zero.
#'
#' @inheritParams step_center
#' @param means A named numeric vector of means. This is `NULL` until computed
#'   by [prep()].
#' @param sds A named numeric vector of standard deviations This is `NULL` until
#'   computed by [prep()].
#' @param na_rm A logical value indicating whether `NA` values should be removed
#'   when computing the standard deviation and mean.
#' @template step-return
#' @family normalization steps
#' @export
#' @details
#'
#' Centering data means that the average of a variable is subtracted from the
#' data. Scaling data means that the standard deviation of a variable is divided
#' out of the data. `step_normalize()` estimates the variable standard
#' deviations and means from the data used in the `training` argument of
#' [prep()]. [bake()] then applies the scaling to new data sets using these
#' estimates.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `statistic`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{statistic}{character, name of statistic (`"mean"` or `"sd"`)}
#'   \item{value}{numeric, value of the `statistic`}
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
#' norm_trans <- rec |>
#'   step_normalize(carbon, hydrogen)
#'
#' norm_obj <- prep(norm_trans, training = biomass_tr)
#'
#' transformed_te <- bake(norm_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' tidy(norm_trans, number = 1)
#' tidy(norm_obj, number = 1)
#'
#' # To keep the original variables in the output, use `step_mutate_at`:
#' norm_keep_orig <- rec |>
#'   step_mutate_at(all_numeric_predictors(), fn = list(orig = ~.)) |>
#'   step_normalize(-contains("orig"), -all_outcomes())
#'
#' keep_orig_obj <- prep(norm_keep_orig, training = biomass_tr)
#' keep_orig_te <- bake(keep_orig_obj, biomass_te)
#' keep_orig_te
step_normalize <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    means = NULL,
    sds = NULL,
    na_rm = TRUE,
    skip = FALSE,
    id = rand_id("normalize")
  ) {
    add_step(
      recipe,
      step_normalize_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        means = means,
        sds = sds,
        na_rm = na_rm,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_normalize_new <-
  function(terms, role, trained, means, sds, na_rm, skip, id, case_weights) {
    step(
      subclass = "normalize",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      sds = sds,
      na_rm = na_rm,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

sd_check <- function(x) {
  zero_sd <- which(x < .Machine$double.eps)
  if (length(zero_sd) > 0) {
    offenders <- names(zero_sd)

    cli::cli_warn(
      c(
        "!" = "{cli::qty(offenders)} The following column{?s} {?has/have} zero \\
            variance so scaling cannot be used: {offenders}.",
        "i" = "Consider using {.help [?step_zv](recipes::step_zv)} to remove \\
            those columns before normalizing."
      )
    )

    x[zero_sd] <- 1
  }

  na_sd <- which(is.na(x))
  if (length(na_sd) > 0) {
    cli::cli_warn(
      "Column{?s} {.var {names(na_sd)}} returned NaN, because variance \\
        cannot be calculated and scaling cannot be used. Consider avoiding \\
        `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before \\
        normalizing."
    )
  }
  x
}

#' @export
prep.step_normalize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$na_rm, arg = "na_rm")

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  means <- averages(training[, col_names], wts, na_rm = x$na_rm)
  vars <- variances(training[, col_names], wts, na_rm = x$na_rm)
  sds <- sqrt(vars)
  sds <- sd_check(sds)

  step_normalize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    sds = sds,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_normalize <- function(object, new_data, ...) {
  col_names <- names(object$means)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    mean <- object$means[col_name]
    sd <- object$sds[col_name]

    new_data[[col_name]] <- (new_data[[col_name]] - mean) / sd
  }

  new_data
}

#' @export
print.step_normalize <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Centering and scaling for "
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
tidy.step_normalize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = c(names(x$means), names(x$sds)),
      statistic = rep(c("mean", "sd"), each = length(x$sds)),
      value = unname(c(x$means, x$sds))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      statistic = na_chr,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}
