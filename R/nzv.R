#' Near-zero variance filter
#'
#' `step_nzv()` creates a *specification* of a recipe step that will potentially
#' remove variables that are highly sparse and unbalanced.
#'
#' @inheritParams step_center
#' @param freq_cut,unique_cut Numeric parameters for the filtering process. See
#'   the Details section below.
#' @param options A list of options for the filter (see Details below).
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @template step-return
#' @template filter-steps
#' @family variable filter steps
#' @export
#'
#' @details
#'
#' This step diagnoses predictors that have one unique value (i.e. are zero
#' variance predictors) or predictors that have both of the following
#' characteristics:
#' \enumerate{
#'   \item they have very few unique values relative to the number
#'    of samples and
#'   \item the ratio of the frequency of the most common value to
#'    the frequency of the second most common value is large.
#' }
#'
#' For example, an example of near-zero variance predictor is one that, for 1000
#' samples, has two distinct values and 999 of them are a single value.
#'
#' To be flagged, first, the frequency of the most prevalent value over the
#' second most frequent value (called the "frequency ratio") must be above
#' `freq_cut`. Secondly, the "percent of unique values," the number of unique
#' values divided by the total number of samples (times 100), must also be below
#' `unique_cut`.
#'
#' In the above example, the frequency ratio is 999 and the unique value percent
#' is 0.2%.
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
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_nzv"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass$sparse <- c(1, rep(0, nrow(biomass) - 1))
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen +
#'   nitrogen + sulfur + sparse,
#' data = biomass_tr
#' )
#'
#' nzv_filter <- rec |>
#'   step_nzv(all_predictors())
#'
#' filter_obj <- prep(nzv_filter, training = biomass_tr)
#'
#' filtered_te <- bake(filter_obj, biomass_te)
#' any(names(filtered_te) == "sparse")
#'
#' tidy(nzv_filter, number = 1)
#' tidy(filter_obj, number = 1)
step_nzv <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    freq_cut = 95 / 5,
    unique_cut = 10,
    options = list(freq_cut = 95 / 5, unique_cut = 10),
    removals = NULL,
    skip = FALSE,
    id = rand_id("nzv")
  ) {
    exp_list <- list(freq_cut = 95 / 5, unique_cut = 10)

    if (!isTRUE(all.equal(exp_list, options))) {
      lifecycle::deprecate_stop(
        "0.1.7",
        "step_nzv(options)",
        details = "Please use the arguments `freq_cut` and `unique_cut` instead."
      )
    }

    add_step(
      recipe,
      step_nzv_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        freq_cut = freq_cut,
        unique_cut = unique_cut,
        options = options,
        removals = removals,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_nzv_new <-
  function(
    terms,
    role,
    trained,
    freq_cut,
    unique_cut,
    options,
    removals,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "nzv",
      terms = terms,
      role = role,
      trained = trained,
      freq_cut = freq_cut,
      unique_cut = unique_cut,
      options = options,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_nzv <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_number_decimal(x$unique_cut, arg = "unique_cut", min = 0, max = 100)
  check_number_decimal(x$freq_cut, arg = "freq_cut", min = 0)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  filter <- nzv(
    x = training[, col_names],
    wts = wts,
    freq_cut = x$freq_cut,
    unique_cut = x$unique_cut
  )

  step_nzv_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    freq_cut = x$freq_cut,
    unique_cut = x$unique_cut,
    options = x$options,
    removals = filter,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_nzv <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_nzv <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      title <- "Sparse, unbalanced variable filter removed "
    } else {
      title <- "Sparse, unbalanced variable filter on "
    }
    print_step(
      x$removals,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

nzv <- function(x, wts, freq_cut = 95 / 5, unique_cut = 10) {
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  }

  fr_foo <- function(data) {
    t <- weighted_table(data[!is.na(data)], wts = wts)
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)

    return(max(t, na.rm = TRUE) / max(t[-w], na.rm = TRUE))
  }

  freq_ratio <- vapply(x, fr_foo, c(ratio = 0))
  uni_foo <- function(data) {
    length(unique(data[!is.na(data)]))
  }
  lunique <- vapply(x, uni_foo, c(num = 0))
  pct_unique <- 100 * lunique / vapply(x, length, c(num = 0))

  zero_func <- function(data) {
    all(is.na(data))
  }
  zero_var <- (lunique == 1) | vapply(x, zero_func, c(zv = TRUE))

  out <-
    which(
      (freq_ratio > freq_cut &
        pct_unique <= unique_cut) |
        zero_var
    )
  names(out) <- NULL
  colnames(x)[out]
}

#' @rdname tidy.recipe
#' @export
tidy.step_nzv <- tidy_filter

#' @export
tunable.step_nzv <- function(x, ...) {
  tibble::tibble(
    name = c("freq_cut", "unique_cut"),
    call_info = list(
      list(pkg = "dials", fun = "freq_cut"),
      list(pkg = "dials", fun = "unique_cut")
    ),
    source = "recipe",
    component = "step_nzv",
    component_id = x$id
  )
}
