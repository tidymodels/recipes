#' High correlation filter
#'
#' `step_corr()` creates a *specification* of a recipe step that will
#' potentially remove variables that have large absolute correlations with other
#' variables.
#'
#' @inheritParams step_center
#' @param threshold A value for the threshold of absolute correlation values.
#'   The step will try to remove the minimum number of columns so that all the
#'   resulting absolute correlations are less than this value.
#' @param use A character string for the `use` argument to the [stats::cor()]
#'   function.
#' @param method A character string for the `method` argument to the
#'   [stats::cor()] function.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @template step-return
#' @template filter-steps
#' @author Original R code for filtering algorithm by Dong Li, modified by Max
#'   Kuhn. Contributions by Reynald Lescarbeau (for original in `caret`
#'   package). Max Kuhn for the `step` function.
#' @family variable filter steps
#' @export
#'
#' @details
#'
#' This step attempts to remove variables to keep the largest absolute
#' correlation between the variables less than `threshold`.
#'
#' When a column has a single unique value, that column will be excluded from
#' the correlation analysis. Also, if the data set has sporadic missing values
#' (and an inappropriate value of `use` is chosen), some columns will also be
#' excluded from the filter.
#'
#' The arguments `use` and `method` don't take effect if case weights are used
#' in the recipe.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected to be removed}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_corr"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' set.seed(3535)
#' biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + duplicate,
#'   data = biomass_tr
#' )
#'
#' corr_filter <- rec |>
#'   step_corr(all_numeric_predictors(), threshold = .5)
#'
#' filter_obj <- prep(corr_filter, training = biomass_tr)
#'
#' filtered_te <- bake(filter_obj, biomass_te)
#' round(abs(cor(biomass_tr[, c(3:7, 9)])), 2)
#' round(abs(cor(filtered_te)), 2)
#'
#' tidy(corr_filter, number = 1)
#' tidy(filter_obj, number = 1)
step_corr <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  use = "pairwise.complete.obs",
  method = "pearson",
  removals = NULL,
  skip = FALSE,
  id = rand_id("corr")
) {
  add_step(
    recipe,
    step_corr_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      threshold = threshold,
      use = use,
      method = method,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_corr_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    use,
    method,
    removals,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "corr",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      use = use,
      method = method,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_corr <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$threshold, min = 0, max = 1, arg = "threshold")
  use <- x$use
  rlang::arg_match(
    use,
    c(
      "all.obs",
      "complete.obs",
      "pairwise.complete.obs",
      "everything",
      "na.or.complete"
    )
  )
  method <- x$method
  rlang::arg_match(method, c("pearson", "kendall", "spearman"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(col_names) > 1) {
    filter <- corr_filter(
      x = training[, col_names],
      wts = wts,
      cutoff = x$threshold,
      use = x$use,
      method = x$method
    )
  } else {
    filter <- character(0)
  }

  step_corr_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    use = x$use,
    method = x$method,
    removals = filter,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_corr <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_corr <-
  function(x, width = max(20, options()$width - 36), ...) {
    title <- "Correlation filter on "
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

corr_filter <-
  function(
    x,
    wts = NULL,
    cutoff = .90,
    use = "pairwise.complete.obs",
    method = "pearson"
  ) {
    x <- correlations(x, wts = wts, use = use, method = method)

    if (any(!vec_detect_complete(x))) {
      all_na <- apply(x, 2, function(x) all(is.na(x)))
      if (sum(all_na) >= nrow(x) - 1) {
        cli::cli_warn(
          "Too many correlations are `NA`; skipping correlation filter."
        )
        return(numeric(0))
      } else {
        na_cols <- which(all_na)
        if (length(na_cols) > 0) {
          x[na_cols, ] <- 0
          x[, na_cols] <- 0
          cli::cli_warn(
            "The correlation matrix has missing values. \\
            {length(na_cols)} column{?s} {?was/were} excluded from the filter."
          )
        }
      }
      if (anyNA(x)) {
        cli::cli_warn(
          "The correlation matrix has sporadic missing values. \\
          Some columns were excluded from the filter."
        )
        x[is.na(x)] <- 0
      }
      diag(x) <- 1
    }
    averageCorr <- colMeans(abs(x))
    averageCorr <- as.numeric(as.factor(averageCorr))
    x[lower.tri(x, diag = TRUE)] <- NA
    combsAboveCutoff <- which(abs(x) > cutoff)

    colsToCheck <- ceiling(combsAboveCutoff / nrow(x))
    rowsToCheck <- combsAboveCutoff %% nrow(x)

    colsToDiscard <- averageCorr[colsToCheck] > averageCorr[rowsToCheck]
    rowsToDiscard <- !colsToDiscard

    deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
    deletecol <- unique(deletecol)
    if (length(deletecol) > 0) {
      deletecol <- colnames(x)[deletecol]
    }
    deletecol
  }

tidy_filter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$removals))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname tidy.recipe
#' @export
tidy.step_corr <- tidy_filter

#' @export
tunable.step_corr <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_corr",
    component_id = x$id
  )
}
