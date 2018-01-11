#' High Correlation Filter
#'
#' `step_corr` creates a *specification* of a recipe
#'  step that will potentially remove variables that have large
#'  absolute correlations with other variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param threshold A value for the threshold of absolute
#'  correlation values. The step will try to remove the minimum
#'  number of columns so that all the resulting absolute
#'  correlations are less than this value.
#' @param use A character string for the `use` argument to
#'  the [stats::cor()] function.
#' @param method A character string for the `method` argument
#'  to the [stats::cor()] function.
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until [prep.recipe()] is called.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be removed.
#' @keywords datagen
#' @author Original R code for filtering algorithm by Dong Li,
#'  modified by Max Kuhn. Contributions by Reynald Lescarbeau (for
#'  original in `caret` package). Max Kuhn for the `step`
#'  function.
#' @concept preprocessing variable_filters
#' @export
#'
#' @details This step attempts to remove variables to keep the
#'  largest absolute correlation between the variables less than
#'  `threshold`.
#' @examples
#' data(biomass)
#'
#' set.seed(3535)
#' biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
#'                     sulfur + duplicate,
#'               data = biomass_tr)
#'
#' corr_filter <- rec %>%
#'   step_corr(all_predictors(), threshold = .5)
#'
#' filter_obj <- prep(corr_filter, training = biomass_tr)
#'
#' filtered_te <- bake(filter_obj, biomass_te)
#' round(abs(cor(biomass_tr[, c(3:7, 9)])), 2)
#' round(abs(cor(filtered_te)), 2)
#'
#' tidy(corr_filter, number = 1)
#' tidy(filter_obj, number = 1)
#' @seealso [step_nzv()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]

step_corr <- function(recipe,
                      ...,
                      role = NA,
                      trained = FALSE,
                      threshold = 0.9,
                      use = "pairwise.complete.obs",
                      method = "pearson",
                      removals = NULL,
                      skip = FALSE) {
  add_step(
    recipe,
    step_corr_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      threshold = threshold,
      use = use,
      method = method,
      removals = removals,
      skip = skip
    )
  )
}

step_corr_new <-
  function(
    terms = NULL,
    role = NA,
    trained = FALSE,
    threshold = NULL,
    use = NULL,
    method = NULL,
    removals = NULL,
    skip = FALSE
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
      skip = skip
    )
  }

#' @export
prep.step_corr <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  filter <- corr_filter(
    x = training[, col_names],
    cutoff = x$threshold,
    use = x$use,
    method = x$method
  )

  step_corr_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    use = x$use,
    method = x$method,
    removals = filter,
    skip = x$skip
  )
}

#' @export
bake.step_corr <- function(object, newdata, ...) {
  if (length(object$removals) > 0)
    newdata <- newdata[,!(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
}

print.step_corr <-
  function(x,  width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Correlation filter removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("Correlation filter removed no terms")
    } else {
      cat("Correlation filter on ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


#' @importFrom stats cor
corr_filter <-
  function(x,
           cutoff = .90,
           use = "pairwise.complete.obs",
           method = "pearson") {
    x <- cor(x, use = use, method = method)

    if (any(!complete.cases(x)))
      stop("The correlation matrix has some missing values.")
    averageCorr <- colMeans(abs(x))
    averageCorr <- as.numeric(as.factor(averageCorr))
    x[lower.tri(x, diag = TRUE)] <- NA
    combsAboveCutoff <- which(abs(x) > cutoff)

    colsToCheck <- ceiling(combsAboveCutoff / nrow(x))
    rowsToCheck <- combsAboveCutoff %% nrow(x)

    colsToDiscard <-
      averageCorr[colsToCheck] > averageCorr[rowsToCheck]
    rowsToDiscard <- !colsToDiscard

    deletecol <-
      c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
    deletecol <- unique(deletecol)
    if (length(deletecol) > 0)
      deletecol <- colnames(x)[deletecol]
    deletecol
  }

tidy_filter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$removals)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = na_chr)
  }
  res
}

#' @rdname step_corr
#' @param x A `step_corr` object.
tidy.step_corr <- tidy_filter
