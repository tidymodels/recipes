#' Data Depths
#'
#' \code{step_depth} creates a a \emph{specification} of a recipe step that
#'   will convert numeric data into measurement of \emph{data depth}. This is
#'   done for each value of a categorical class variable.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables that
#'   will be used to create the new features. See \code{\link{selections}} for
#'   more details.
#' @param class A single character string that specifies a single categorical
#'   variable to be used as the class.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that resulting depth
#'   estimates will be used as predictors in a model.
#' @param metric A character string specifying the depth metric. Possible
#'   values are "potential", "halfspace", "Mahalanobis", "simplicialVolume",
#'   "spatial", and "zonoid".
#' @param options A list of options to pass to the underlying depth functions.
#'   See \code{\link[ddalpha]{depth.halfspace}},
#'   \code{\link[ddalpha]{depth.Mahalanobis}},
#'   \code{\link[ddalpha]{depth.potential}},
#'   \code{\link[ddalpha]{depth.projection}},
#'   \code{\link[ddalpha]{depth.simplicial}},
#'   \code{\link[ddalpha]{depth.simplicialVolume}},
#'   \code{\link[ddalpha]{depth.spatial}}, \code{\link[ddalpha]{depth.zonoid}}.
#' @param data The training data are stored here once after
#' \code{\link{prep.recipe}} is executed.
#' @keywords datagen
#' @concept preprocessing dimension_reduction
#' @export
#' @details Data depth metrics attempt to measure how close data a data point
#'   is to the center of its distribution.  There are a number of methods for
#'   calculating death but a simple example is the inverse of the distance of
#'   a data point to the centroid of the distribution. Generally, small values
#'   indicate that a data point not close to the centroid. \code{step_depth}
#'   can compute a class-specific depth for a new data point based on the
#'   proximity of the new value to the training set distribution.
#'
#' Note that the entire training set is saved to compute future depth values.
#' The saved data have been trained (i.e. prepared) and baked (i.e. processed) up to the point before the
#' location that \code{step_depth} occupies in the recipe. Also, the data
#' requirements for the different step methods may vary. For example, using
#' \code{metric = "Mahalanobis"} requires that each class should have at least
#' as many rows as variables listed in the \code{terms} argument.
#'
#' The function will create a new column for every unique value of the
#' \code{class} variable. The resulting variables will not replace the
#' original values and have the prefix \code{depth_}.
#'
#' @examples
#'
#' # halfspace depth is the default
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_depth(all_predictors(), class = "Species")
#'
#' rec_dists <- prep(rec, training = iris)
#'
#' dists_to_species <- bake(rec_dists, newdata = iris)
#' dists_to_species

step_depth <-
  function(recipe,
           ...,
           class,
           role = "predictor",
           trained = FALSE,
           metric =  "halfspace",
           options = list(),
           data = NULL) {
    if (!is.character(class) || length(class) != 1)
      stop("`class` should be a single character value.")
    add_step(
      recipe,
      step_depth_new(
        terms = check_ellipses(...),
        class = class,
        role = role,
        trained = trained,
        metric = metric,
        options = options,
        data = data
      )
    )
  }

step_depth_new <-
  function(terms = NULL,
           class = NULL,
           role = "predictor",
           trained = FALSE,
           metric = NULL,
           options = NULL,
           data = NULL) {
    step(
      subclass = "depth",
      terms = terms,
      class = class,
      role = role,
      trained = trained,
      metric = metric,
      options = options,
      data = data
    )
  }

#' @importFrom stats as.formula model.frame
#' @export
prep.step_depth <- function(x, training, info = NULL, ...) {
  class_var <- x$class[1]
  x_names <- terms_select(x$terms, info = info)
  x_dat <-
    split(training[, x_names], getElement(training, class_var))
  x_dat <- lapply(x_dat, as.matrix)
  step_depth_new(
    terms = x$terms,
    class = x$class,
    role = x$role,
    trained = TRUE,
    metric = x$metric,
    options = x$options,
    data = x_dat
  )
}


get_depth <- function(tr_dat, new_dat, metric, opts) {
  if (!is.matrix(new_dat))
    new_dat <- as.matrix(new_dat)
  opts$data <- tr_dat
  opts$x <- new_dat
  do.call(paste0("depth.", metric), opts)
}



#' @importFrom tibble as_tibble
#' @importFrom ddalpha depth.halfspace depth.Mahalanobis depth.potential
#'   depth.projection depth.simplicial depth.simplicialVolume depth.spatial
#'   depth.zonoid
#' @export
bake.step_depth <- function(object, newdata, ...) {
  x_names <- colnames(object$data[[1]])
  x_data <- as.matrix(newdata[, x_names])
  res <- lapply(
    object$data,
    get_depth,
    new_dat = x_data,
    metric = object$metric,
    opts = object$options
  )
  res <- as_tibble(res)
  colnames(res) <- paste0("depth_", colnames(res))
  res <- cbind(newdata, res)
  if (!is_tibble(res))
    res <- as_tibble(res)
  res
}

print.step_depth <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Data depth by ", x$class, "for ")
    
    if (x$trained) {
      cat(format_ch_vec(x_names, width = width))
    } else
      x_names <- NULL
    printer(x_names, x$terms, x$trained, width = width)
    invisible(x)
  }
