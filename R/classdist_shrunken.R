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
#' @family multivariate transformation steps
#' @export
#' @details `step_classdist` will create a new column for every
#'  unique value of the `class` variable.
#'  The resulting variables will not replace the original values
#'  and by default have the prefix `classdist_` using the `keep_original_cols`
#'  argument is used. The naming format can be changed using the `prefix` argument.

# TODO add prior?
step_classdist_shrunken <-
    function(recipe,
             ...,
             class = NULL,
             role = NA,
             trained = FALSE,
             threshold = 1 / 2,
             prefix = "classdist_",
             keep_original_cols = FALSE,
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
          prefix = prefix,
          keep_original_cols = keep_original_cols,
          objects = objects,
          skip = skip,
          id = id
        )
      )
    }

step_classdist_shrunken_new <-
  function(terms, class, trained, role, threshold, prefix, keep_original_cols,
           objects, na_rm, skip, id) {
    step(
      subclass = "classdist_shrunken",
      terms = terms,
      class = class,
      role = role,
      trained = trained,
      threshold = threshold,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      objects = objects,
      skip = skip,
      id = id
    )
  }

# ------------------------------------------------------------------------------

compute_centroids <- function(x, y = NULL, prefix = "classdist_") {
  x <- tibble::as_tibble(x)
  if (is.null(y)) {
    x$.class <- "group"
    n_overall <- nrow(x)
  } else {
    x$.class <- y
    n_overall <- length(y)
  }

  res <-
    tidyr::pivot_longer(x, cols = c(-.class), names_to = "variable") %>%
    dplyr::group_by(variable, .class) %>%
    dplyr::summarize(
      mean = mean(value, na.rm = TRUE),
      std_dev = sd(value, na.rm = TRUE),
      n = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ssq = std_dev^2 * (n_overall - 1),
      m = sqrt(1/n + 1/n_overall),
      prob = n / n_overall
    )
  if (is.null(y)) {
    res$.class <- NULL
    res <- dplyr::rename_with(res, ~ paste0(.x, "_global"), c(-variable))
  } else {
    res <-
      dplyr::rename_with(res, ~ paste0(.x, "_class"), c(-variable, -.class)) %>%
      dplyr::mutate(.class = paste0(prefix, .class))
  }
  res
}

# TODO add case weights
compute_shrunken_centroids <- function(x, y, threshold, prefix = "classdist_") {
  cent_global <- compute_centroids(x, prefix = prefix)
  cent_class <- compute_centroids(x, y, prefix = prefix)
  num_lvl <- length(levels(y))
  num_data <- length(y)
  pooled_std <-
    cent_class %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(std_dev_pooled = sum(ssq_class), .groups = "drop") %>%
    dplyr::mutate(std_dev_pooled = sqrt(std_dev_pooled / (num_data - num_lvl)))
  cent_global <- dplyr::full_join(cent_global, pooled_std, by = "variable")

  centroids <-
    cent_class %>%
    dplyr::left_join(
      cent_global %>% select(mean_global, std_dev_pooled, variable),
      by = "variable"
    ) %>%
    dplyr::mutate(
      SNR_class = (mean_class - mean_global) / (m_class * std_dev_class),
      max_delta = max(abs(SNR_class))
      )
  max_SNR <- centroids$SNR_class[1]
  threshold <- threshold * max_SNR

  shrunken <-
    centroids %>%
    dplyr::mutate(
      threshold = !!threshold,
      shrink = abs(SNR_class) - threshold,
      mean_shrunk = sign(SNR_class) * ifelse(shrink > 0, shrink, 0),
      centroid = mean_class + (mean_shrunk * m_class * std_dev_pooled)
    )
  shrunken %>%
    dplyr::select(variable, .class, std_dev_pooled, centroid, prob_class, max_delta)
}

new_shrunken_scores <- function(object, new_data) {
  preds <- unique(unique(object$variable))
  new_data %>%
    dplyr::select(dplyr::all_of(preds)) %>%
    parsnip::add_rowindex() %>%
    tidyr::pivot_longer(c(-.row), names_to = "variable", values_to = "value") %>%
    dplyr::right_join(object, by = "variable", multiple = "all") %>%
    dplyr::mutate(score = ( (value - centroid)^2 / std_dev_pooled^2 ) - 2 * log(prob_class) ) %>%
    dplyr::group_by(.class, .row) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(id_cols = ".row", names_from = .class, values_from = score) %>%
    dplyr::select(-.row)
}
# ------------------------------------------------------------------------------


prep.step_classdist_shrunken <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  y_names <- recipes_eval_select(x$class, training, info)

  check_type(training[, x_names], types = c("double", "integer"))
  check_type(training[, y_names], types = c("factor"))

  # TODO check threshold
  stats <-
    compute_shrunken_centroids(
      training[, x_names],
      training[[ y_names]],
      x$threshold,
      x$prefix
    )

  step_classdist_shrunken_new(
    terms = x$terms,
    class = x$class,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    prefix = x$prefix,
    keep_original_cols = x$keep_original_cols,
    objects = stats,
    skip = x$skip,
    id = x$id
  )
}

bake.step_classdist_shrunken <- function(object, new_data, ...) {
  new_cols <- new_shrunken_scores(object$objects, new_data)
  if (!object$keep_original_cols) {
    preds <- unique(object$objects$variable)
    new_data <- new_data[, !(names(new_data) %in% preds)]
  }
  as_tibble(dplyr::bind_cols(new_data, new_cols))
}

print.step_classdist_shrunken <-
  function(x, width = max(20, options()$width - 30), ...) {
    preds <- unique(object$stats$variable)
    title <- "Shrunken centroid with"
    print_step(preds, x$terms, x$trained, title, width)
    invisible(x)
  }


# TODO fill this out
#' @rdname tidy.recipe
#' @export
tidy.step_classdist_shrunken <- function(x, ...) {
  if (is_trained(x)) {
    res <-
2
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}


#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_classdist_shrunken <- function(x, ...) {
  c("dplyr", "tidyr")
}


