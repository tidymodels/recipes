#' Using case weights with recipes
#'
#' Case weights are positive numeric values that may influence how much each
#' data point has during the preprocessing. There are a variety of situations
#' where case weights can be used.
#'
#' tidymodels packages differentiate _how_ different types of case weights
#' should be used during the entire data analysis process, including
#' preprocessing data, model fitting, performance calculations, etc.
#'
#' The tidymodels packages require users to convert their numeric vectors to a
#' vector class that reflects how these should be used. For example, there are
#' some situations where the weights should not affect operations such as
#' centering and scaling or other preprocessing operations.
#'
#' The types of weights allowed in tidymodels are:
#'
#' * Frequency weights via [hardhat::frequency_weights()]
#' * Importance weights via [hardhat::importance_weights()]
#'
#' More types can be added by request.
#'
#' For recipes, we distinguish between supervised and unsupervised steps.
#' Supervised steps use the outcome in the calculations, this type of steps will
#' use frequency and importance weights. Unsupervised steps don't use the
#' outcome and will only use frequency weights.
#'
#' There are 3 main principles about how case weights are used within recipes.
#' First, the data set that is passed to the `recipe()` function should already
#' have a case weights column in it. This column can be created beforehand using
#' [hardhat::frequency_weights()] or [hardhat::importance_weights()]. Second,
#' There can only be 1 case weights column in a recipe at any given time. Third,
#' You can not modify the case weights column with most of the steps or using
#' the `update_role()` and `add_role()` functions.
#'
#' These principles ensure that you experience minimal surprises when using case
#' weights, as the steps automatically apply case weighted operations when
#' supported. The printing method will additionally show which steps where
#' weighted and which steps ignored the weights because they were of an
#' incompatible type.
#'
#' @name case_weights
#' @seealso [frequency_weights()], [importance_weights()]
NULL

#' Helpers for steps with case weights
#'
#' These functions can be used to do basic calculations with or without case
#' weights.
#'
#' @param info A data frame from the `info` argument within steps
#' @param .data The training data
#' @param x A numeric vector or a data frame
#' @param wts A vector of case weights
#' @param na_rm A logical value indicating whether `NA` values should be removed
#'   during computations.
#' @param use Used by [correlations()] or [covariances()] to pass argument to
#'   [cor()] or [cov()]
#' @param method Used by [correlations()] or [covariances()] to pass argument to
#'   [cor()] or [cov()]
#' @param unsupervised Can the step handle unsupervised weights
#' @inheritParams rlang::args_error_context
#' @details
#'
#' [get_case_weights()] is designed for developers of recipe steps, to return a
#' column with the role of "case weight" as a vector.
#'
#' For the other functions, rows with missing case weights are removed from
#' calculations.
#'
#' For `averages()` and `variances()`, missing values in the data (*not* the
#' case weights) only affect the calculations for those rows. For
#' `correlations()`, the correlation matrix computation first removes rows with
#' any missing values (equal to the "complete.obs" strategy in [stats::cor()]).
#'
#' `are_weights_used()` is designed for developers of recipe steps and is used
#' inside print method to determine how printing should be done.
#'
#' @seealso [developer_functions]
#'
#' @export
#' @name case-weight-helpers
get_case_weights <- function(info, .data, call = rlang::caller_env()) {
  wt_col <- info$variable[info$role == "case_weights" & !is.na(info$role)]

  if (length(wt_col) == 1) {
    res <- .data[[wt_col]]
    if (!is.numeric(res)) {
      cli::cli_abort(
        c(
          x = "{.field {wt_col}} has a {.code case_weights} role and should be
              numeric, but is {.obj_type_friendly {wt_col}}.",
          i = "Only numeric case weights are supported in recipes."
        ),
        call = call
      )
    }
  } else if (length(wt_col) == 0) {
    res <- NULL
  } else {
    too_many_case_weights(wt_col, call = call)
  }

  res
}

# ------------------------------------------------------------------------------

too_many_case_weights <- function(x, call = rlang::caller_env()) {
  n <- length(x)

  cli::cli_abort(
    c(
      "!" = "There should only be a single column with the role \\
      {.code case_weights}.",
      "i" = "In these data, there are {n} columns: {.var {x}}."
    ),
    call = call
  )
}

# ------------------------------------------------------------------------------

wt_calcs <- function(x, wts, statistic = "mean") {
  statistic <- rlang::arg_match(
    statistic,
    c("mean", "var", "cor", "cov", "pca", "median")
  )
  if (!is.data.frame(x)) {
    x <- data.frame(x)
  }

  if (is.null(wts)) {
    wts <- rep(1L, nrow(x))
  }

  complete <- vec_detect_complete(x) & !is.na(wts)
  wts <- wts[complete]
  x <- x[complete, , drop = FALSE]
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
averages <- function(x, wts = NULL, na_rm = TRUE) {
  if (NCOL(x) == 0) {
    return(vapply(x, mean, c(mean = 0), na.rm = TRUE))
  }
  if (is.null(wts)) {
    res <- colMeans(x, na.rm = TRUE)
  } else {
    wts <- as.double(wts)
    res <- purrr::map_dbl(x, wt_calcs, wts = wts)
  }
  if (!na_rm) {
    res[map_lgl(x, anyNA)] <- NA
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
    wts <- as.double(wts)
    res <- purrr::map_dbl(x, wt_calcs, wts = wts, statistic = "median")
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
    res <- purrr::map_dbl(x, stats::var, na.rm = na_rm)
  } else {
    wts <- as.double(wts)
    res <- purrr::map_dbl(x, wt_calcs, wts = wts, statistic = "var")
    if (!na_rm) {
      res[map_lgl(x, anyNA)] <- NA
    }
  }
  res
}

#' @export
#' @rdname case-weight-helpers
correlations <- function(
  x,
  wts = NULL,
  use = "everything",
  method = "pearson"
) {
  if (is.null(wts)) {
    res <- stats::cor(x, use = use, method = method)
  } else {
    wts <- as.double(wts)
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
    wts <- as.double(wts)
    res <- wt_calcs(x, wts, statistic = "cov")
  }
  res
}

#' @export
#' @rdname case-weight-helpers
pca_wts <- function(x, wts = NULL) {
  wts <- as.double(wts)
  res <- wt_calcs(x, wts, statistic = "pca")
  res$center <- FALSE
  res$scale <- FALSE
  rownames(res$rotation) <- names(x)
  res
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

is_unsupervised_weights <- function(wts) {
  if (!hardhat::is_case_weights(wts)) {
    cli::cli_abort("Must be be a {.code case_weights} variable.")
  }

  hardhat::is_frequency_weights(wts)
}

#' @export
#' @rdname case-weight-helpers
are_weights_used <- function(wts, unsupervised = FALSE) {
  if (is.null(wts)) {
    return(NULL)
  }

  if (unsupervised) {
    return(is_unsupervised_weights(wts))
  }

  TRUE
}
