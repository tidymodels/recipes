#' Percentile Transformation
#'
#' `step_percentile` creates a *specification* of a recipe step that
#' replaces the value of a variable with its percentile from the training set.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ref_dist A prefix for generated column names, defaults to "right_relu_"
#'   for right hinge transformation and "left_relu_" for reversed/left hinge
#'   transformations.
#' @param options A named list of options to pass to [stats::quantile()].
#'   See Details for more information.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @rdname step_percentile
#'
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr) %>%
#'   step_percentile(carbon)
#'
#' prepped_rec <- prep(rec)
#'
#' prepped_rec %>%
#'   bake(biomass_te)
#'
#' tidy(rec, 1)
#' tidy(prepped_rec, 1)
step_percentile <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           ref_dist = NULL,
           options = list(probs = (0:100)/100, names = TRUE),
           skip = FALSE,
           id = rand_id("percentile")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_percentile_new(
      terms = terms,
      trained = trained,
      role = role,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_percentile_new <-
  function(terms, role, trained, ref_dist, options, skip, id) {
    step(
      subclass = "percentile",
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_percentile <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  ## You can add error trapping for non-numeric data here and so on.

  ## We'll use the names later so make sure they are available
  if (x$options$names == FALSE) {
    rlang::abort("`names` should be set to TRUE")
  }

  if (!any(names(x$options) == "probs")) {
    x$options$probs <- (0:100)/100
  } else {
    x$options$probs <- sort(unique(x$options$probs))
  }

  # Compute percentile grid
  ref_dist <- purrr::map(training[, col_names],  get_train_pctl, args = x$options)

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_percentile_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_dist = ref_dist,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

get_train_pctl <- function(x, args = NULL) {
  res <- rlang::exec("quantile", x = x, !!!args)
  # Remove duplicate percentile values
  res[!duplicated(res)]
}

#' @export
bake.step_percentile <- function(object, new_data, ...) {
  ## For illustration (and not speed), we will loop through the affected variables
  ## and do the computations
  vars <- names(object$ref_dist)

  new_data[, vars] <-
    purrr::map2_dfc(new_data[, vars], object$ref_dist, pctl_by_approx)

  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}

pctl_by_approx <- function(x, ref) {
  # In case duplicates were removed, get the percentiles from
  # the names of the reference object
  grid <- as.numeric(gsub("%$", "", names(ref)))
  stats::approx(x = ref, y = grid, xout = x)$y/100
}

print.step_percentile <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Percentile transformation on ", sep = "")
    printer(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = names(x$ref_dist),
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_percentile <- function(x, ...) {
  if (is_trained(x)) {
    res <- map_dfr(x$ref_dist, format_pctl, .id = "term")
  }
  else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        value = rlang::na_dbl,
        percentile = rlang::na_dbl
      )
  }
  # Always return the step id:
  res$id <- x$id
  res
}

format_pctl <- function(x) {
  tibble::tibble(
    value = unname(x),
    percentile = as.numeric(gsub("%$", "", names(x)))
  )
}
