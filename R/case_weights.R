#' Helpers for steps with case weights
#'
#' These functions can be used to do basic calculations with or without case
#' weights.
#'
#' @param selection A quosure
#' @param info A data frame from the `info` argument within steps
#' @param .data The training data
#' @param x A numeric vector or a data frame
#' @param wts A vector of case weights
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
get_case_weights <- function(selection, info, .data) {
  if (all(purrr::map_lgl(selection, quo_is_null))) {
    return(NULL)
  }
  wt_col <- unname(recipes_eval_select(selection, .data, info))

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
  statistic <- rlang::arg_match(statistic, c("mean", "var", "cor", "pca"))
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
  } else if (statistic == "var") {
    res <- unname(diag(res[["cov"]]))
  } else if (statistic == "pca") {
    res <- cov2pca(res$cov)
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

#' @param useNA whether to include NA values in the table.
#'   Loosely mimics [table()].
#' @export
#' @rdname case-weight-helpers
weighted_table <- function(.data, wts = NULL, useNA = "no") {
  if (!is.data.frame(.data)) {
    if (is.factor(.data)) {
      .data <- data.frame(.data = .data)
    } else {
      .data <- data.frame(.data = factor(.data))
    }
  }

  if (!all(purrr::map_lgl(.data, is.factor))) {
    rlang::abort("All columns in `.data` must be factors.")
  }

  if (is.null(wts)) {
    return(table(.data))
  }

  data <- .data %>%
    mutate(wts = wts) %>%
    group_by(dplyr::across(c(-wts)), .drop = FALSE) %>%
    summarise(n = sum(wts), .groups = "drop") %>%
    ungroup()

  if (useNA == "no") {
    missing <- purrr::map(data, is.na) %>% purrr::reduce(`|`)
    data <- data[!missing, ]
  }

  var_names <- names(data)[seq_len(length(data) - 1)]
  tab <- table(.data[var_names], dnn = var_names, useNA = useNA)
  combinations <- expand.grid(attr(tab, "dimnames"))
  names(combinations) <- var_names

  data_order <- combinations %>%
    dplyr::right_join(
      by = var_names,
      data %>% mutate(.row_number = dplyr::row_number())
    )

  tab[seq_along(tab)] <- data$n[data_order$.row_number]

  tab
}

#' @export
#' @rdname case-weight-helpers
is_unsupervised_weights <- function(wts) {
  if (!hardhat::is_case_weights(wts)) {
    rlang::abort("Must be be a case_weights variable")
  }

  hardhat::is_frequency_weights(wts)
}
