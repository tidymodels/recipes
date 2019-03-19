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
#'
#' When a column has a single unique value, that column will be
#'  excluded from the correlation analysis. Also, if the data set
#'  has sporadic missing values (and an inappropriate value of `use`
#'  is chosen), some columns will also be excluded from the filter.
#'
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
                      skip = FALSE,
                      id = rand_id("corr")
                      ) {
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
      skip = skip,
      id = id
    )
  )
}

step_corr_new <-
  function(terms, role, trained, threshold, use, method, removals, skip, id) {
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
      id = id
    )
  }

#' @export
prep.step_corr <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  if (length(col_names) > 1) {
    filter <- corr_filter(
      x = training[, col_names],
      cutoff = x$threshold,
      use = x$use,
      method = x$method
    )
  } else {
    filter <- numeric(0)
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
    id = x$id
  )
}

#' @export
bake.step_corr <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[,!(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
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

    if (any(!complete.cases(x))) {
      all_na <- apply(x, 2, function(x) all(is.na(x)))
      if (sum(all_na) >= nrow(x) - 1) {
        warning("Too many correlations are `NA`; skipping correlation filter.",
                call. = FALSE)
        return(numeric(0))
      } else {
        na_cols <- which(all_na)
        if (length(na_cols) >  0) {
          x[na_cols, ] <- 0
          x[, na_cols] <- 0
          warning("The correlation matrix has missing values. ",
                  length(na_cols), " columns were excluded from the filter.",
                  call. = FALSE)
        }
      }
      if (any(is.na(x))) {
        warning("The correlation matrix has sporadic missing values. ",
                "Some columns were excluded from the filter.",
                call. = FALSE)
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
    res <- tibble(terms = x$removals)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = na_chr)
  }
  res$id <- x$id
  res
}

#' @rdname step_corr
#' @param x A `step_corr` object.
#' @export
tidy.step_corr <- tidy_filter
