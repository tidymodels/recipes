#' Distances to Class Centroids
#'
#' `step_classdist` creates a *specification* of a
#'  recipe step that will convert numeric data into Mahalanobis
#'  distance measurements to the data centroid. This is done for
#'  each value of a categorical class variable.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param class A single character string that specifies a single
#'  categorical variable to be used as the class.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that resulting distances will be used as predictors in a model.
#' @param mean_func A function to compute the center of the
#'  distribution.
#' @param cov_func A function that computes the covariance matrix
#' @param pool A logical: should the covariance matrix be computed
#'  by pooling the data for all of the classes?
#' @param log A logical: should the distances be transformed by
#'  the natural log function?
#' @param objects Statistics are stored here once this step has
#'  been trained by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the centroid of
#'  the class), and `class`.
#' @keywords datagen
#' @concept preprocessing dimension_reduction
#' @export
#' @details `step_classdist` will create a new column for every
#'  unique value of the `class` variable.
#'  The resulting variables will not replace the original values
#'  and have the prefix `classdist_`.
#'
#' Note that, by default, the default covariance function requires
#'  that each class should have at least as many rows as variables
#'  listed in the `terms` argument. If `pool = TRUE`,
#'  there must be at least as many data points are variables
#'  overall.
#' @examples
#'
#' # in case of missing data...
#' mean2 <- function(x) mean(x, na.rm = TRUE)
#'
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_classdist(all_predictors(), class = "Species",
#'                  pool = FALSE, mean_func = mean2)
#'
#' rec_dists <- prep(rec, training = iris)
#'
#' dists_to_species <- bake(rec_dists, new_data = iris, everything())
#' ## on log scale:
#' dist_cols <- grep("classdist", names(dists_to_species), value = TRUE)
#' dists_to_species[, c("Species", dist_cols)]
#'
#' tidy(rec, number = 1)
#' tidy(rec_dists, number = 1)
#' @importFrom stats cov
step_classdist <- function(recipe,
                           ...,
                           class,
                           role = "predictor",
                           trained = FALSE,
                           mean_func = mean,
                           cov_func = cov,
                           pool = FALSE,
                           log = TRUE,
                           objects = NULL,
                           skip = FALSE,
                           id = rand_id("classdist")) {
  if (!is.character(class) || length(class) != 1)
    stop("`class` should be a single character value.")
  add_step(
    recipe,
    step_classdist_new(
      terms = ellipse_check(...),
      class = class,
      role = role,
      trained = trained,
      mean_func = mean_func,
      cov_func = cov_func,
      pool = pool,
      log = log,
      objects = objects,
      skip = skip,
      id = id
    )
  )
}

step_classdist_new <-
  function(terms, class, role, trained, mean_func,
           cov_func, pool, log, objects, skip, id) {
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
      skip = skip,
      id = id
    )
  }

get_center <- function(x, mfun = mean) {
  apply(x, 2, mfun)
}
get_both <- function(x, mfun = mean, cfun = cov) {
  list(center = get_center(x, mfun),
       scale = cfun(x))
}


#' @importFrom stats as.formula model.frame
#' @export
prep.step_classdist <- function(x, training, info = NULL, ...) {
  class_var <- x$class[1]
  x_names <- terms_select(x$terms, info = info)
  check_type(training[, x_names])

  x_dat <-
    split(training[, x_names], getElement(training, class_var))
  if (x$pool) {
    res <- list(
      center = lapply(x_dat, get_center, mfun = x$mean_func),
      scale = x$cov_func(training[, x_names])
    )

  } else {
    res <-
      lapply(x_dat,
             get_both,
             mfun = x$mean_func,
             cfun = x$cov_func)
  }
  step_classdist_new(
    terms = x$terms,
    class = x$class,
    role = x$role,
    trained = TRUE,
    mean_func = x$mean_func,
    cov_func = x$cov_func,
    pool = x$pool,
    log = x$log,
    objects = res,
    skip = x$skip,
    id = x$id
  )
}


#' @importFrom stats mahalanobis
mah_by_class <- function(param, x)
  mahalanobis(x, param$center, param$scale)

mah_pooled <- function(means, x, cov_mat)
  mahalanobis(x, means, cov_mat)


#' @importFrom tibble as_tibble
#' @export
bake.step_classdist <- function(object, new_data, ...) {
  if (object$pool) {
    x_cols <- names(object$objects[["center"]][[1]])
    res <- lapply(
      object$objects$center,
      mah_pooled,
      x = new_data[, x_cols],
      cov_mat = object$objects$scale
    )
  } else {
    x_cols <- names(object$objects[[1]]$center)
    res <-
      lapply(object$objects, mah_by_class, x = new_data[, x_cols])
  }
  if (object$log)
    res <- lapply(res, log)
  res <- as_tibble(res)
  newname <- paste0("classdist_", colnames(res))
  res <- check_name(res, new_data, object, newname)
  res <- bind_cols(new_data, res)
  if (!is_tibble(res))
    res <- as_tibble(res)
  res
}

print.step_classdist <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Distances to", x$class, "for ")
    if (x$trained) {
      x_names <- if (x$pool)
        names(x$objects[["center"]][[1]])
      else
        names(x$objects[[1]]$center)
    } else x_names <- NULL
    printer(x_names, x$terms, x$trained, width = width)
    invisible(x)
  }



get_centroid <- function(x) {
  tibble(terms = names(x$center),
         value = unname(x$center))
}

#' @rdname step_classdist
#' @param x A `step_classdist` object.
#' @importFrom dplyr bind_rows
#' @export
tidy.step_classdist <- function(x, ...) {
  if (is_trained(x)) {
    centroids <- lapply(x$objects, get_centroid)
    num_rows <- vapply(centroids, nrow, numeric(1))
    classes <- rep(names(centroids), num_rows)
    res <- dplyr::bind_rows(centroids)
    res$class <- classes
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl,
                  class = na_chr)
  }
  res$id <- x$id
  res
}
