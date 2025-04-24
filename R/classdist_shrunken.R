#' Compute shrunken centroid distances for classification models
#'
#' `step_classdist_shrunken()` creates a *specification* of a recipe step  that
#' will convert numeric data into Euclidean distance to the regularized class
#' centroid. This is done for each value of a categorical class variable.
#'
#' @inheritParams step_center
#' @inheritParams step_classdist
#' @param threshold A regularization parameter between zero and one. Zero means
#'   that no regularization is used and one means that centroids should be
#'   shrunk to the global centroid.
#' @param sd_offset A value between zero and one for the quantile that should be
#'   used to stabilize the pooled standard deviation.
#' @family multivariate transformation steps
#'
#' @details
#'
#' Class-specific centroids are the multivariate averages of each predictor
#' using the data from each class in the training set. When pre-processing a new
#' data point, this step computes the distance from the new point to each of the
#' class centroids. These distance features can be very effective at capturing
#' linear class boundaries. For this reason, they can be useful to add to an
#' existing predictor set used within a nonlinear model. If the true boundary is
#' actually linear, the model will have an easier time learning the training
#' data patterns.
#'
#' Shrunken centroids use a form of regularization where the class-specific
#' centroids are contracted to the overall class-independent centroid. If a
#' predictor is uninformative, shrinking it may move it entirely to the overall
#' centroid. This has the effect of removing that predictor's effect on the new
#' distance features. However, it may not move all of the class-specific
#' features to the center in many cases. This means that some features will only
#' affect the classification of specific classes.
#'
#' The `threshold` parameter can be used to optimized how much regularization
#' should be used.
#'
#' `step_classdist_shrunken()` will create a new column for every unique value
#' of the `class` variable. The resulting variables will not replace the
#' original values and, by default, have the prefix `classdist_`. The naming
#' format can be changed using the `prefix` argument.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, `class`, `type`, `threshold` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the centroid}
#'   \item{class}{character, name of class variable}
#'   \item{type}{character, has values `"global"`, `"by_class"`, and `"shrunken"`}
#'   \item{threshold}{numeric, value of threshold}
#'   \item{id}{character, id of this step}
#' }
#'
#' The first two types of centroids are in the original units while the last
#' has been standardized.
#'
#' @template case-weights-supervised
#' @references
#' Tibshirani, R., Hastie, T., Narasimhan, B., & Chu, G. (2002). Diagnosis of
#' multiple cancer types by shrunken centroids of gene expression. _Proceedings
#' of the National Academy of Sciences_, 99(10), 6567-6572.
#' @examplesIf rlang::is_installed(c("modeldata"))
#' data(penguins, package = "modeldata")
#' penguins <- penguins[vctrs::vec_detect_complete(penguins), ]
#' penguins$island <- NULL
#' penguins$sex <- NULL
#'
#' # define naming convention
#' rec <- recipe(species ~ ., data = penguins) |>
#'   step_classdist_shrunken(all_numeric_predictors(),
#'     class = species,
#'     threshold = 1 / 4, prefix = "centroid_"
#'   )
#'
#' # default naming
#' rec <- recipe(species ~ ., data = penguins) |>
#'   step_classdist_shrunken(all_numeric_predictors(),
#'     class = species,
#'     threshold = 3 / 4
#'   )
#'
#' rec_dists <- prep(rec, training = penguins)
#'
#' dists_to_species <- bake(rec_dists, new_data = penguins)
#' ## on log scale:
#' dist_cols <- grep("classdist", names(dists_to_species), value = TRUE)
#' dists_to_species[, c("species", dist_cols)]
#'
#' tidy(rec, number = 1)
#' tidy(rec_dists, number = 1)
#' @export
step_classdist_shrunken <-
  function(
    recipe,
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
    id = rand_id("classdist_shrunken")
  ) {
    recipes_pkg_check(required_pkgs.step_classdist_shrunken())

    add_step(
      recipe,
      step_classdist_shrunken_new(
        terms = enquos(...),
        class = enquos(class),
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
  function(
    terms,
    class,
    trained,
    role,
    threshold,
    sd_offset,
    log,
    prefix,
    keep_original_cols,
    objects,
    na_rm,
    skip,
    id
  ) {
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
  } else {
    wts <- as.numeric(wts)
  }
  mns <- apply(x, 2, wt_calcs, wts = wts)
  dplyr::tibble(variable = names(mns), global = unname(mns))
}

centroid_class <- function(x, y, wts = NULL, sd_offset = 1 / 2) {
  x <- tibble::as_tibble(x)
  x$..y <- as.character(y)
  if (is.null(wts)) {
    wts <- rep(1, nrow(x))
  } else {
    wts <- as.numeric(wts)
  }
  x$..wts <- wts
  num_class <- length(levels(y))
  n <- sum(x$..wts)

  # ------------------------------------------------------------------------------

  x <- tidyr::pivot_longer(x, cols = c(-..y, -..wts), names_to = "variable")

  centroids <-
    x |>
    dplyr::summarize(
      by_class = stats::weighted.mean(value, ..wts, na.rm = TRUE),
      class_n = sum(..wts),
      .by = c(variable, ..y)
    )

  # ------------------------------------------------------------------------------

  std_dev_est <-
    centroids[, c("variable", "..y", "by_class")] |>
    dplyr::full_join(x, by = c("variable", "..y")) |>
    dplyr::mutate(sq_df = (value - by_class)^2) |>
    dplyr::summarize(
      # use mean * n for weighted sum
      msq = (stats::weighted.mean(sq_df, ..wts, na.rm = TRUE) * n) /
        (n - num_class),
      .by = "variable"
    ) |>
    dplyr::mutate(std_dev = sqrt(msq))
  std_dev_off <- stats::quantile(std_dev_est$std_dev, prob = sd_offset)
  std_dev_est$std_dev <- std_dev_est$std_dev + unname(std_dev_off)

  # ------------------------------------------------------------------------------

  dplyr::full_join(centroids, std_dev_est, by = c("variable")) |>
    dplyr::select(variable, class = ..y, by_class, std_dev, class_n)
}

compute_shrunken_centroids <- function(
  x,
  y,
  wts = NULL,
  threshold = 1 / 2,
  sd_offset = 1 / 2
) {
  cent_global <- centroid_global(x, wts)
  cent_class <- centroid_class(x, y, wts, sd_offset = sd_offset)
  if (is.null(wts)) {
    wts <- rep(1, nrow(x))
  } else {
    wts <- as.numeric(wts)
  }
  num_class <- length(table(y))
  wts_sum <- sum(wts)

  centroids <-
    dplyr::full_join(cent_global, cent_class, by = "variable") |>
    dplyr::mutate(
      delta = (by_class - global) / std_dev,
      delta_wts = sqrt((1 / class_n) - (1 / !!wts_sum)),
      delta = delta / delta_wts
    )

  max_delta <- max(centroids$delta)
  threshold <- threshold * max_delta

  shrunken <-
    centroids |>
    dplyr::mutate(
      threshold = !!threshold,
      shrink = abs(delta) - threshold,
      shrunken = sign(delta) * ifelse(shrink > 0, shrink, 0) * delta_wts
    )

  shrunken <-
    shrunken |>
    dplyr::select(variable, class, global, by_class, shrunken, std_dev)
  list(centroids = shrunken, threshold = threshold, max_delta = max_delta)
}

new_shrunken_scores <- function(
  object,
  new_data,
  prefix = "classdist_",
  log = TRUE
) {
  preds <- unique(unique(object$variable))
  res <-
    new_data |>
    dplyr::select(dplyr::all_of(preds)) |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(
      c(-.row),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::right_join(
      object,
      by = "variable",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      scaled_value = (value - global) / std_dev,
      sq_diff = (scaled_value - shrunken)^2
    ) |>
    dplyr::summarize(
      distance = sum(sq_diff),
      .by = c(.row, class)
    )

  if (log) {
    res$distance <- log(res$distance)
  }

  res <-
    res |>
    tidyr::pivot_wider(
      id_cols = ".row",
      names_from = class,
      values_from = distance
    ) |>
    dplyr::select(-.row)

  names(res) <- paste0(prefix, names(res))
  res
}
# ------------------------------------------------------------------------------

#' @export
prep.step_classdist_shrunken <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  y_names <- recipes_argument_select(
    x$class,
    training,
    info,
    arg_name = "class"
  )

  check_type(training[, x_names], types = c("double", "integer"))
  check_type(training[, y_names], types = c("factor"))

  threshold <- x$threshold
  check_number_decimal(threshold, min = 0, max = 1)

  sd_offset <- x$sd_offset
  check_number_decimal(sd_offset, min = 0, max = 1)
  check_bool(x$log)
  check_string(x$prefix)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(x_names) > 0) {
    stats <-
      compute_shrunken_centroids(
        x = training[, x_names],
        y = training[[y_names]],
        wts = wts,
        threshold = x$threshold,
        sd_offset = x$sd_offset
      )
  } else {
    stats <- list(
      centroids = tibble::tibble(
        variable = character(),
        class = character(),
        global = double(),
        by_class = double(),
        shrunken = double(),
        std_dev = double()
      )
    )
  }

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

#' @export
bake.step_classdist_shrunken <- function(object, new_data, ...) {
  col_names <- unique(object$objects$variable)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  new_cols <-
    new_shrunken_scores(
      object$objects,
      new_data |> dplyr::select(dplyr::all_of(col_names)),
      object$prefix,
      object$log
    )
  new_cols <- check_name(new_cols, new_data, object, names(new_cols))
  new_data <- vctrs::vec_cbind(new_data, new_cols, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
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
      x$object |>
      dplyr::select(terms = variable, class, global, by_class, shrunken) |>
      tidyr::pivot_longer(
        cols = c(global, by_class, shrunken),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::relocate(terms, value, class, type) |>
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

#' @export
tunable.step_classdist_shrunken <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0, 0.1))
    ),
    source = "recipe",
    component = "step_classdist_shrunken",
    component_id = x$id
  )
}
