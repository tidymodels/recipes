#' Helpers for steps with case weights
#'
#' These functions can be used to do basic calculations with or without case
#' weights.
#'
#' @param info A data frame from the `info` argument within steps.
#' @param .data The training data
#' @param x A numeric vector or a data frame.
#' @param wts A vector of case weights.
#' @details
#' [get_case_weights()] is designed for developers to use so that any column
#' with the role of "case weight" will be returned as a vector.
#'
#' For the other functions:
#'
#' Rows with missing case weights are removed from all calculations.
#'
#' For means and variances, the missing values in the data (not case weights)
#' only affect the calculations for those rows. For the correlation matrix, the
#' calculation first removes rows with any missing values (equal to the
#' "complete.obs" strategy in [stats::cor()]).
#' @export
#' @name case-weight-helpers
get_case_weights <- function(info, .data) {
  wt_ind <- which(info$role == "case weight")
  if (length(wt_ind) == 1) {
    wt_var <- info$variable[wt_ind]
    res <- .data[[wt_var]]
    if (!is.numeric(res)) {
      rlang::abort(
        "Column ", wt_var, " has a 'case weight' role but is not numeric."
      )
    }
  } else if (length(wt_ind) == 0) {
    res <- NULL
  } else {
    rlang::abort(
      "There should only be a single column with the role 'case-weight'. ",
      "In these data, there are ", length(wt_ind), "columns."
    )
  }

  res
}

# ------------------------------------------------------------------------------

wt_calcs <- function(x, wts, statistic = "mean") {
  statistic <- rlang::arg_match(statistic, c("mean", "var", "cor"))
  if (!is.data.frame(x)) {
    x <- data.frame(x)
  }
  complete <- stats::complete.cases(x) & !is.na(wts)
  wts <- wts[complete]
  x <- x[complete,,drop = FALSE]
  res <- stats::cov.wt(x, wt = wts, cor = statistic == "cor")

  if (statistic == "mean") {
    res <- unname(res[["center"]])
  } else if (statistic == "var") {
    res <- unname(diag(res[["cov"]]))
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
    res <- purrr::map_dbl(x, ~ wt_calcs(.x, wts))
  }
  res
}

#' @export
#' @rdname case-weight-helpers
variances <- function(x, wts = NULL) {
  if (NCOL(x) == 0) {
    return(vapply(x, sd, c(sd = 0), na.rm = TRUE))
  }
  if (is.null(wts)) {
    res <- purrr::map_dbl(x, ~ stats::var(.x, na.rm = TRUE))
  } else {
    res <- purrr::map_dbl(x, ~ wt_calcs(.x, wts, statistic = "var"))
  }
  res
}

#' @export
#' @rdname case-weight-helpers
correlations <- function(x, wts = NULL) {
  if (is.null(wts)) {
    res <- stats::cor(x, use = "pairwise.complete.obs")
  } else {
    res <- wt_calcs(x, wts, statistic = "cor")
  }
  res
}


