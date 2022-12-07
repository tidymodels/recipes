#' Percentile Transformation
#'
#' `step_percentile` creates a *specification* of a recipe step that
#' replaces the value of a variable with its percentile from the training set.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ref_dist The computed percentiles is stored here once this
#'  preprocessing step has be trained by [prep()].
#' @param options A named list of options to pass to [stats::quantile()].
#'   See Details for more information.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @rdname step_percentile
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
#' ) %>%
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
           options = list(probs = (0:100) / 100),
           skip = FALSE,
           id = rand_id("percentile")) {
    add_step(
      recipe,
      step_percentile_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        ref_dist = ref_dist,
        options = options,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_percentile_new <-
  function(terms, role, trained, ref_dist, options, skip, id, case_weights) {
    step(
      subclass = "percentile",
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_percentile <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  ## We'll use the names later so make sure they are available
  x$options$names <- TRUE

  if (!any(names(x$options) == "probs")) {
    x$options$probs <- (0:100) / 100
  } else {
    x$options$probs <- sort(unique(x$options$probs))
  }

  ref_dist <- purrr::map(
    training[, col_names],
    get_train_pctl,
    wts = wts,
    args = x$options
  )

  step_percentile_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_dist = ref_dist,
    options = x$options,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

get_train_pctl <- function(x, wts, args = NULL) {
  if (is.null(wts)) {
    res <- rlang::exec("quantile", x = x, !!!args)
  } else {
    wts <- as.double(wts)
    res <- rlang::exec("wrighted_quantile", x = x, wts = wts, !!!args)
  }

  # Remove duplicate percentile values
  res[!duplicated(res)]
}

wrighted_quantile <- function(x, wts, probs, ...) {
  order_x <- order(x)
  x <- x[order_x]
  wts <- wts[order_x]

  wts_norm <- cumsum(wts) / sum(wts)
  res <- purrr::map_dbl(probs, ~x[min(which(wts_norm >= .x))])

  names(res) <- paste0(probs * 100, "%")
  res
}

#' @export
bake.step_percentile <- function(object, new_data, ...) {
  vars <- names(object$ref_dist)
  check_new_data(vars, object, new_data)

  new_data[, vars] <-
    purrr::map2_dfc(new_data[, vars], object$ref_dist, pctl_by_approx)

  new_data
}

pctl_by_approx <- function(x, ref) {
  # In case duplicates were removed, get the percentiles from
  # the names of the reference object
  grid <- as.numeric(gsub("%$", "", names(ref)))
  stats::approx(x = ref, y = grid, xout = x)$y / 100
}

print.step_percentile <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Percentile transformation on "
    print_step(names(x$ref_dist), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_percentile <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$ref_dist) == 0) {
      res <- tibble(
        terms = character(),
        value = numeric(),
        percentile = numeric()
      )
    } else {
      res <- map_dfr(x$ref_dist, format_pctl, .id = "term")
    }
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        value = rlang::na_dbl,
        percentile = rlang::na_dbl
      )
  }
  res$id <- x$id
  res
}

format_pctl <- function(x) {
  tibble::tibble(
    value = unname(x),
    percentile = as.numeric(gsub("%$", "", names(x)))
  )
}
