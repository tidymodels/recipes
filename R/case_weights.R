#' Helpers for steps with case weights
#'
#' These functions can be used to do basic calculations with or without case
#' weights.
#'
#' @param info A data frame from the `info` argument within steps
#' @param .data The training data
#' @param x A numeric vector or a data frame
#' @param wts A vector of case weights
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed during computations.
#' @param use Used by [correlations()] or [covariances()] to pass argument to
#'   [cor()] or [cov()]
#' @param method Used by [correlations()] or [covariances()] to pass argument to
#'   [cor()] or [cov()]
#' @details
#' [get_case_weights()] is designed for developers of recipe steps, to return
#' a column with the role of "case weight" as a vector.
#'
#' For the other functions, rows with missing case weights are removed from
#' calculations.
#'
#' For `averages()` and `variances()`, missing values in the data (*not* the
#' case weights) only affect the calculations for those rows. For
#' `correlations()`, the correlation matrix computation first removes rows
#' with any missing values (equal to the "complete.obs" strategy in
#' [stats::cor()]).
#'
#' `is_unsupervised_weights()`
#' @export
#' @name case-weight-helpers
get_case_weights <- function(info, .data) {
  wt_col <- info$variable[info$role == "case_weights" & !is.na(info$role)]


  if (length(wt_col) == 1) {
    res <- .data[[wt_col]]
    if (!is.numeric(res)) {
      rlang::abort(
        paste0(
          "Column ", wt_col, " has a 'case_weights' role but is not numeric."
        )
      )
    }
  } else if (length(wt_col) == 0) {
    res <- NULL
  } else {
    too_many_case_weights(length(wt_col))
  }

  res
}

# ------------------------------------------------------------------------------

too_many_case_weights <- function(n) {
  rlang::abort(
    paste0(
      "There should only be a single column with the role 'case_weights'. ",
      "In these data, there are ", n, " columns."
    )
  )
}

# ------------------------------------------------------------------------------

wt_calcs <- function(x, wts, statistic = "mean") {
  statistic <- rlang::arg_match(statistic, c("mean", "var", "cor", "cov", "pca", "median"))
  if (!is.data.frame(x)) {
    x <- data.frame(x)
  }

  if (is.null(wts)) {
    wts <- rep(1L, nrow(x))
  }

  complete <- stats::complete.cases(x) & !is.na(wts)
  wts <- wts[complete]
  x <- x[complete,,drop = FALSE]
  res <- stats::cov.wt(x, wt = wts, cor = statistic == "cor")

  if (statistic == "mean") {
    res <- unname(res[["center"]])
  } else if (statistic == "median") {
    res <- weighted_median_impl(x$x, wts)
  } else if (statistic == "var") {
    res <- unname(diag(res[["cov"]]))
  } else if (statistic == "pca") {
    res <- cov2pca(res$cov)
  } else if (statistic == "cov") {
    res <- res[["cov"]]
  } else {
    res <- res[["cor"]]
  }
  res
}

#' @export
#' @rdname case-weight-helpers
averages <- function(x, wts = NULL) {
  if (NCOL(x) == 0) {
    return(vapply(x, mean, c(mean = 0), na.rm = TRUE))
  }
  if (is.null(wts)) {
    res <- colMeans(x, na.rm = TRUE)
  } else {
    wts <- as.numeric(wts)
    res <- purrr::map_dbl(x, ~ wt_calcs(.x, wts))
  }
  res
}

#' @export
#' @rdname case-weight-helpers
medians <- function(x, wts = NULL) {
  if (NCOL(x) == 0) {
    return(vapply(x, median, c(median = 0), na.rm = TRUE))
  }
  if (is.null(wts)) {
    res <- apply(x, 2, median, na.rm = TRUE)
  } else {
    wts <- as.numeric(wts)
    res <- purrr::map_dbl(x, ~ wt_calcs(.x, wts, statistic = "median"))
  }
  res
}

weighted_median_impl <- function(x, wts) {
  order_x <- order(x)

  x <- x[order_x]
  wts <- wts[order_x]

  wts_norm <- cumsum(wts) / sum(wts)
  ps <- min(which(wts_norm > 0.5))
  x[ps]
}

#' @export
#' @rdname case-weight-helpers
variances <- function(x, wts = NULL, na_rm = TRUE) {
  if (NCOL(x) == 0) {
    return(vapply(x, sd, c(sd = 0), na.rm = na_rm))
  }
  if (is.null(wts)) {
    res <- purrr::map_dbl(x, ~ stats::var(.x, na.rm = na_rm))
  } else {
    wts <- as.numeric(wts)
    res <- purrr::map_dbl(x, ~ wt_calcs(.x, wts, statistic = "var"))
    if (!na_rm) {
      res[map_lgl(x, ~any(is.na(.x)))] <- NA
    }
  }
  res
}

#' @export
#' @rdname case-weight-helpers
correlations <- function(x, wts = NULL, use = "everything", method = "pearson") {
  if (is.null(wts)) {
    res <- stats::cor(x, use = use, method = method)
  } else {
    wts <- as.numeric(wts)
    res <- wt_calcs(x, wts, statistic = "cor")
  }
  res
}

#' @export
#' @rdname case-weight-helpers
covariances <- function(x, wts = NULL, use = "everything", method = "pearson") {
  if (is.null(wts)) {
    res <- stats::cov(x, use = use, method = method)
  } else {
    wts <- as.numeric(wts)
    res <- wt_calcs(x, wts, statistic = "cov")
  }
  res
}


#' @export
#' @rdname case-weight-helpers
pca_wts <- function(x, wts = NULL) {
  cv_mat <- variances(x, wts)
  cov2pca(cv_mat)
}

cov2pca <- function(cv_mat) {
  res <- eigen(cv_mat)

  # emulate prcomp results
  list(sdev = sqrt(res$values), rotation = res$vectors)
}

weighted_table <- function(x, wts = NULL) {
  if (is.null(wts)) {
    wts <- rep(1, length(x))
  }

  if (!is.factor(x)) {
    x <- factor(x)
  }

  hardhat::weighted_table(x, weights = wts)
}

#' @export
#' @rdname case-weight-helpers
is_unsupervised_weights <- function(wts) {
  if (!hardhat::is_case_weights(wts)) {
    rlang::abort("Must be be a case_weights variable")
  }

  hardhat::is_frequency_weights(wts)
}
