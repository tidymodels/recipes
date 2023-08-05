#' Compute shrunken centroid distances for classification models
#'
#' `step_classdist_shrunken` creates a *specification* of a recipe
#'  step  that will convert numeric data into regularized Euclidean distance
#'  to the class centroid. This is done for each value of a categorical class
#'  variable.
#' @inheritParams step_center
#' @inheritParams step_classdist
#' @param threshold A regularization parameter between zero and one. Zero means
#' that no regularization is used and one means that centroids should be
#' shrunk to the global centroid.
#' @param sd_offset A value between zero and one for the quantile that should
#' be used to stabilize the pooled standard deviation.
#' @family multivariate transformation steps
#' @details `step_classdist_shrunken` will create a new column for every unique value of
#' the `class` variable. The resulting variables will not replace the original
#' values and, by default, have the prefix `classdist_`. The naming format can
#' be changed using the `prefix` argument.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected), `value` (the centroid),
#' `class`, and `type` is returned. Type has values `"global"`, `"by_class"`,
#' and `"shrunken"`. The first two types of centroids are in the original units
#' while the last have been standardized.
#'
#' @template case-weights-supervised
#' @export
step_classdist_shrunken <-
    function(recipe,
             ...,
             class = NULL,
             role = NA,
             trained = FALSE,
             threshold = 1 / 2,
             sd_offset = 1 / 2,
             log = TRUE,
             prefix = "classdist_",
             keep_original_cols = TRUE,
             objects = NULL,
             skip = FALSE,
             id = rand_id("classdist_shrunken")) {

      recipes_pkg_check(required_pkgs.step_classdist_shrunken())

      add_step(
        recipe,
        step_classdist_shrunken_new(
          terms = enquos(...),
          class = class,
          trained = trained,
          role = role,
          threshold = threshold,
          sd_offset = sd_offset,
          log = log,
          prefix = prefix,
          keep_original_cols = keep_original_cols,
          objects = objects,
          skip = skip,
          id = id
        )
      )
    }

step_classdist_shrunken_new <-
  function(terms, class, trained, role, threshold, sd_offset, log, prefix,
           keep_original_cols, objects, na_rm, skip, id) {
    step(
      subclass = "classdist_shrunken",
      terms = terms,
      class = class,
      role = role,
      trained = trained,
      threshold = threshold,
      sd_offset = sd_offset,
      log = log,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      objects = objects,
      skip = skip,
      id = id
    )
  }

# ------------------------------------------------------------------------------

centroid_global <- function(x, wts = NULL) {
  x <- tibble::as_tibble(x)
  if (is.null(wts)) {
    wts <- rep(1, nrow(x))
  }
  mns <- apply(x, 2, recipes:::wt_calcs, wts = wts)
  dplyr::tibble(variable = names(mns), global = unname(mns))
}

centroid_class <- function(x, y, wts = NULL, sd_offset = 1 / 2) {
  x <- tibble::as_tibble(x)
  x$..y <- as.character(y)
  if (is.null(wts)) {
    wts <- rep(1, nrow(x))
  }
  x$..wts <- wts
  num_class <- length(levels(y))
  n <- sum(x$..wts)

  # ------------------------------------------------------------------------------

  x <- tidyr::pivot_longer(x, cols = c(-..y, -..wts), names_to = "variable")

  centroids <-
    x %>%
    dplyr::summarize(
      by_class = stats::weighted.mean(value, ..wts, na.rm = TRUE),
      class_n  = sum(..wts),
      .by = c(variable, ..y)
    )

  # ------------------------------------------------------------------------------

  std_dev_est <-
    centroids[, c("variable", "..y", "by_class")] %>%
    dplyr::full_join(x, by = c("variable", "..y")) %>%
    dplyr::mutate(sq_df  = (value - by_class)^2) %>%
    dplyr::summarize(
      # use mean * n for weighted sum
      msq = ( stats::weighted.mean(sq_df, ..wts, na.rm = TRUE) * n ) /
            (n - num_class),
      .by = "variable"
    ) %>%
    dplyr::mutate(std_dev = sqrt(msq))
  std_dev_off <- stats::quantile(std_dev_est$std_dev, prob = sd_offset)
  std_dev_est$std_dev <- std_dev_est$std_dev + unname(std_dev_off)

  # ------------------------------------------------------------------------------

  dplyr::full_join(centroids, std_dev_est, by = c("variable")) %>%
    dplyr::select(variable, class = ..y, by_class, std_dev, class_n)

}


compute_shrunken_centroids <- function(x, y, wts = NULL, threshold = 1 / 2,
                                       sd_offset = 1 / 2) {
  cent_global <- centroid_global(x, wts)
  cent_class <- centroid_class(x, y, wts, sd_offset = sd_offset)
  if (is.null(wts)) {
    wts <- rep(1, nrow(x))
  }
  num_class <- length(table(y))
  wts_sum <- sum(wts)

  centroids <-
    dplyr::full_join(cent_global, cent_class, by = "variable") %>%
    dplyr::mutate(
      delta = (by_class - global) / std_dev,
      delta_wts = sqrt( ( 1 / class_n ) - ( 1 / !!wts_sum) ),
      delta = delta / delta_wts
    )


  max_delta <- max(centroids$delta)
  threshold <- threshold * max_delta

  shrunken <-
    centroids %>%
    dplyr::mutate(
      threshold = !!threshold,
      shrink = abs(delta) - threshold,
      shrunken = sign(delta) * ifelse(shrink > 0, shrink, 0) * delta_wts
    )

  shrunken <-
    shrunken %>%
    dplyr::select(variable, class, global, by_class, shrunken, std_dev)
  list(centroids = shrunken, threshold = threshold, max_delta = max_delta)
}

new_shrunken_scores <- function(object, new_data, prefix = "classdist_", log = TRUE) {
  preds <- unique(unique(object$variable))
  res <-
    new_data %>%
    dplyr::select(dplyr::all_of(preds)) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(c(-.row), names_to = "variable", values_to = "value") %>%
    dplyr::right_join(object, by = "variable", relationship = "many-to-many") %>%
    dplyr::mutate(
      scaled_value = ( value - global ) / std_dev,
      sq_diff = (scaled_value - shrunken)^2
    ) %>%
    dplyr::summarize(
      distance = sum(sq_diff),
      .by = c(.row, class)
    )

  if (log) {
    res$distance <- log(res$distance)
  }

  res <-
    res %>%
    tidyr::pivot_wider(id_cols = ".row", names_from = class, values_from = distance) %>%
    dplyr::select(-.row)

  names(res) <- paste0(prefix, names(res))
  res
}
# ------------------------------------------------------------------------------


prep.step_classdist_shrunken <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  y_names <- recipes_eval_select(x$class, training, info)

  check_type(training[, x_names], types = c("double", "integer"))
  check_type(training[, y_names], types = c("factor"))

  threshold <- x$threshold
  stopifnot(all(threshold >= 0) & all(threshold <= 1) &
              length(threshold) == 1 & all(!is.na(threshold)))

  sd_offset <- x$sd_offset
  stopifnot(all(sd_offset >= 0) & all(sd_offset <= 1) &
              length(sd_offset) == 1 & all(!is.na(sd_offset)))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  stats <-
    compute_shrunken_centroids(
      x = training[, x_names],
      y = training[[ y_names]],
      wts = wts,
      threshold = x$threshold,
      sd_offset = x$sd_offset
    )

  step_classdist_shrunken_new(
    terms = x$terms,
    class = x$class,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    sd_offset = x$sd_offset,
    log = x$log,
    prefix = x$prefix,
    keep_original_cols = x$keep_original_cols,
    objects = stats$centroids,
    skip = x$skip,
    id = x$id
  )
}

bake.step_classdist_shrunken <- function(object, new_data, ...) {
  pred_vars <- unique(object$objects$variable)
  new_cols <-
    new_shrunken_scores(object$objects,
                        new_data %>% dplyr::select(dplyr::all_of(pred_vars)),
                        object$prefix, object$log)
  if (!object$keep_original_cols) {
    preds <- unique(object$objects$variable)
    new_data <- new_data[, !(names(new_data) %in% preds)]
  }
  as_tibble(dplyr::bind_cols(new_data, new_cols))
}

print.step_classdist_shrunken <-
  function(x, width = max(20, options()$width - 30), ...) {
    preds <- unique(x$objects$variable)
    title <- "Distance to shrunken centroids with"
    print_step(preds, x$terms, x$trained, title, width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_classdist_shrunken <- function(x, ...) {
  if (is_trained(x)) {
    res <-
      x$object %>%
      dplyr::select(terms = variable, class, global, by_class, shrunken) %>%
      tidyr::pivot_longer(
        cols = c(global, by_class, shrunken),
        names_to = "type",
        values_to = "value"
      ) %>%
      dplyr::mutate(threshold = x$threshold)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl,
      class = na_chr,
      type = na_chr,
      threshold = na_dbl
    )
  }
  res$id <- x$id
  res
}


#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_classdist_shrunken <- function(x, ...) {
  c("dplyr", "tidyr")
}


