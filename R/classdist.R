#' Distances to class centroids
#'
#' `step_classdist()` creates a *specification* of a recipe step that will
#' convert numeric data into Mahalanobis distance measurements to the data
#' centroid. This is done for each value of a categorical class variable.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param class A bare name that specifies a single categorical variable to be
#'   used as the class. Can also be a string or tidyselect for backwards
#'   compatibility.
#' @param mean_func A function to compute the center of the distribution.
#' @param cov_func A function that computes the covariance matrix
#' @param pool A logical: should the covariance matrix be computed by pooling
#'   the data for all of the classes?
#' @param log A logical: should the distances be transformed by the natural log
#'   function?
#' @param objects Statistics are stored here once this step has been trained by
#'   [prep()].
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `TRUE`.
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#'
#' `step_classdist()` will create a new column for every unique value of the
#' `class` variable. The resulting variables will not replace the original
#' values and, by default, have the prefix `classdist_`. The naming format can
#' be changed using the `prefix` argument.
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
#' Note that, by default, the default covariance function requires that each
#' class should have at least as many rows as variables listed in the `terms`
#' argument. If `pool = TRUE`, there must be at least as many data points are
#' variables overall.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, `class` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, location of centroid}
#'   \item{class}{character, name of the class}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-supervised
#'
#' @examplesIf rlang::is_installed(c("modeldata"))
#' data(penguins, package = "modeldata")
#' penguins <- penguins[vctrs::vec_detect_complete(penguins), ]
#' penguins$island <- NULL
#' penguins$sex <- NULL
#'
#' # in case of missing data...
#' mean2 <- function(x) mean(x, na.rm = TRUE)
#'
#' # define naming convention
#' rec <- recipe(species ~ ., data = penguins) |>
#'   step_classdist(all_numeric_predictors(),
#'     class = species,
#'     pool = FALSE, mean_func = mean2, prefix = "centroid_"
#'   )
#'
#' # default naming
#' rec <- recipe(species ~ ., data = penguins) |>
#'   step_classdist(all_numeric_predictors(),
#'     class = species,
#'     pool = FALSE, mean_func = mean2
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
step_classdist <- function(
  recipe,
  ...,
  class,
  role = "predictor",
  trained = FALSE,
  mean_func = mean,
  cov_func = cov,
  pool = FALSE,
  log = TRUE,
  objects = NULL,
  prefix = "classdist_",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("classdist")
) {
  add_step(
    recipe,
    step_classdist_new(
      terms = enquos(...),
      class = enquos(class),
      role = role,
      trained = trained,
      mean_func = mean_func,
      cov_func = cov_func,
      pool = pool,
      log = log,
      objects = objects,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_classdist_new <-
  function(
    terms,
    class,
    role,
    trained,
    mean_func,
    cov_func,
    pool,
    log,
    objects,
    prefix,
    keep_original_cols,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "classdist",
      terms = terms,
      class = class,
      role = role,
      trained = trained,
      mean_func = mean_func,
      cov_func = cov_func,
      pool = pool,
      log = log,
      objects = objects,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

get_center <- function(x, wts = NULL, mfun = mean) {
  if (!is.null(wts) & !identical(mfun, mean)) {
    cli::cli_abort(
      "The centering function requested cannot be used with case weights."
    )
  }
  x <- tibble::as_tibble(x)
  if (is.null(wts)) {
    res <- vapply(x, FUN = mfun, FUN.VALUE = numeric(1))
  } else {
    res <- averages(x, wts)
  }
  res
}

get_both <- function(x, wts = NULL, mfun = mean, cfun = cov) {
  if (!is.null(wts) & !identical(mfun, mean)) {
    cli::cli_abort(
      "The centering function requested cannot be used with case weights."
    )
  }
  if (!is.null(wts) & !identical(cfun, cov)) {
    cli::cli_abort(
      "The variance function requested cannot be used with case weights."
    )
  }

  if (is.null(wts)) {
    res <- list(center = get_center(x, wts = wts, mfun = mfun), scale = cfun(x))
  } else {
    res <- list(center = averages(x, wts), scale = cov.wt(x, wts)$cov)
  }
  res
}

#' @export
prep.step_classdist <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  class_var <- recipes_argument_select(
    x$class,
    training,
    info,
    arg_name = "class"
  )
  check_type(training[, x_names], types = c("double", "integer"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  check_function(x$mean_func)
  check_function(x$cov_func)
  check_bool(x$pool)
  check_string(x$prefix)

  x_dat <- split(training[, x_names], training[[class_var]])
  if (is.null(wts)) {
    wts_split <- map(x_dat, \(x) NULL)
  } else {
    wts_split <- split(as.double(wts), training[[class_var]])
  }
  if (x$pool) {
    res <- list(
      center = purrr::map2(x_dat, wts_split, get_center, mfun = x$mean_func),
      scale = covariances(training[, x_names], wts = wts)
    )
  } else {
    res <-
      purrr::map2(
        x_dat,
        wts_split,
        get_both,
        mfun = x$mean_func,
        cfun = x$cov_func
      )
  }
  step_classdist_new(
    terms = x$terms,
    class = class_var,
    role = x$role,
    trained = TRUE,
    mean_func = x$mean_func,
    cov_func = x$cov_func,
    pool = x$pool,
    log = x$log,
    objects = res,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

mah_by_class <- function(param, x) {
  if (ncol(x) == 0L) {
    # mahalanobis() can't handle 0 column case
    return(rep(NA_real_, nrow(x)))
  }

  mahalanobis(x, param$center, param$scale)
}

mah_pooled <- function(means, x, cov_mat) {
  if (ncol(x) == 0L) {
    # mahalanobis() can't handle 0 column case
    return(rep(NA_real_, nrow(x)))
  }

  mahalanobis(x, means, cov_mat)
}

#' @export
bake.step_classdist <- function(object, new_data, ...) {
  col_names <- names(object$objects[[1]][[1]])
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  if (object$pool) {
    new_values <- lapply(
      object$objects$center,
      mah_pooled,
      x = new_data[, col_names],
      cov_mat = object$objects$scale
    )
  } else {
    new_values <- lapply(
      object$objects,
      mah_by_class,
      x = new_data[, col_names]
    )
  }

  if (object$log) {
    new_values <- lapply(new_values, log)
  }
  new_values <- tibble::new_tibble(new_values)

  new_names <- paste0(object$prefix, colnames(new_values))
  colnames(new_values) <- new_names

  new_values <- check_name(new_values, new_data, object, new_names)
  new_data <- vctrs::vec_cbind(new_data, new_values, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_classdist <-
  function(x, width = max(20, options()$width - 30), ...) {
    if (x$trained) {
      title <- glue("Distances to {x$class} for ")
      x_names <- if (x$pool) {
        names(x$objects[["center"]][[1]])
      } else {
        names(x$objects[[1]]$center)
      }
    } else {
      class <- rlang::quo_name(x$class[[1]])
      title <- glue("Distances to {class} for ")
      x_names <- NULL
    }
    print_step(
      x_names,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

get_centroid <- function(x) {
  tibble(
    terms = names(x$center),
    value = unname(x$center)
  )
}
get_centroid_pool <- function(x) {
  tibble(terms = names(x), value = unname(x))
}

#' @rdname tidy.recipe
#' @export
tidy.step_classdist <- function(x, ...) {
  if (is_trained(x)) {
    if (x$pool) {
      centroids <- lapply(x$objects$center, get_centroid_pool)
    } else {
      centroids <- lapply(x$objects, get_centroid)
    }
    num_rows <- vapply(centroids, nrow, numeric(1))
    classes <- rep(names(centroids), num_rows)
    res <- dplyr::bind_rows(centroids)
    res$class <- classes
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl,
      class = na_chr
    )
  }
  res$id <- x$id
  res
}
