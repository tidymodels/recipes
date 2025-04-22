#' Data depths
#'
#' `step_depth()` creates a *specification* of a recipe step that will convert
#' numeric data into a measurement of *data depth*. This is done for each value
#' of a categorical class variable.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param metric A character string specifying the depth metric. Possible values
#'   are `"potential"`, `"halfspace"`, `"Mahalanobis"`, `"simplicialVolume"`,
#'   `"spatial"`, and `"zonoid"`.
#' @param options A list of options to pass to the underlying depth functions.
#'   See [ddalpha::depth.halfspace()], [ddalpha::depth.Mahalanobis()],
#'   [ddalpha::depth.potential()], [ddalpha::depth.projection()],
#'   [ddalpha::depth.simplicial()], [ddalpha::depth.simplicialVolume()],
#'   [ddalpha::depth.spatial()], [ddalpha::depth.zonoid()].
#' @param data The training data are stored here once after [prep()] is
#'   executed.
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#'
#' Data depth metrics attempt to measure how close data a data point is to the
#' center of its distribution. There are a number of methods for calculating
#' depth but a simple example is the inverse of the distance of a data point to
#' the centroid of the distribution. Generally, small values indicate that a
#' data point not close to the centroid. `step_depth()` can compute a
#' class-specific depth for a new data point based on the proximity of the new
#' value to the training set distribution.
#'
#' This step requires the \pkg{ddalpha} package. If not installed, the step will
#' stop with a note about installing the package.
#'
#' Note that the entire training set is saved to compute future depth values.
#' The saved data have been trained (i.e. prepared) and baked (i.e. processed)
#' up to the point before the location that `step_depth()` occupies in the
#' recipe. Also, the data requirements for the different step methods may vary.
#' For example, using `metric = "Mahalanobis"` requires that each class should
#' have at least as many rows as variables listed in the `terms` argument.
#'
#' The function will create a new column for every unique value of the `class`
#' variable. The resulting variables will not replace the original values and by
#' default have the prefix `depth_`. The naming format can be changed using the
#' `prefix` argument.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `class` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{class}{character, name of class variable}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("ddalpha")
#'
#' # halfspace depth is the default
#' rec <- recipe(Species ~ ., data = iris) |>
#'   step_depth(all_numeric_predictors(), class = Species)
#'
#' # use zonoid metric instead
#' # also, define naming convention for new columns
#' rec <- recipe(Species ~ ., data = iris) |>
#'   step_depth(all_numeric_predictors(),
#'     class = Species,
#'     metric = "zonoid", prefix = "zonoid_"
#'   )
#'
#' rec_dists <- prep(rec, training = iris)
#'
#' dists_to_species <- bake(rec_dists, new_data = iris)
#' dists_to_species
#'
#' tidy(rec, number = 1)
#' tidy(rec_dists, number = 1)
step_depth <-
  function(
    recipe,
    ...,
    class,
    role = "predictor",
    trained = FALSE,
    metric = "halfspace",
    options = list(),
    data = NULL,
    prefix = "depth_",
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("depth")
  ) {
    recipes_pkg_check(required_pkgs.step_depth())

    add_step(
      recipe,
      step_depth_new(
        terms = enquos(...),
        class = enquos(class),
        role = role,
        trained = trained,
        metric = metric,
        options = options,
        data = data,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_depth_new <-
  function(
    terms,
    class,
    role,
    trained,
    metric,
    options,
    data,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "depth",
      terms = terms,
      class = class,
      role = role,
      trained = trained,
      metric = metric,
      options = options,
      data = data,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

depth_metric <- c(
  "potential",
  "halfspace",
  "Mahalanobis",
  "simplicialVolume",
  "spatial",
  "zonoid"
)

#' @export
prep.step_depth <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  class_var <- recipes_argument_select(x$class, training, info)
  check_type(training[, x_names], types = c("double", "integer"))
  metric <- x$metric
  rlang::arg_match(metric, depth_metric)
  check_string(x$prefix, allow_empty = FALSE, arg = "prefix")
  check_options(x$options, exclude = c("data", "x"))

  x_dat <- split(training[, x_names], training[[class_var]])
  x_dat <- lapply(x_dat, as.matrix)
  step_depth_new(
    terms = x$terms,
    class = class_var,
    role = x$role,
    trained = TRUE,
    metric = x$metric,
    options = x$options,
    data = x_dat,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

get_depth <- function(tr_dat, new_dat, metric, opts) {
  if (ncol(new_dat) == 0L || nrow(new_dat) == 0L) {
    # ddalpha can't handle 0 col inputs or 0 row inputs
    return(rep(NA_real_, nrow(new_dat)))
  }

  if (!is.matrix(new_dat)) {
    new_dat <- as.matrix(new_dat)
  }
  opts$data <- tr_dat
  opts$x <- new_dat
  dd_call <- call2(paste0("depth.", metric), !!!opts, .ns = "ddalpha")
  eval(dd_call)
}

#' @export
bake.step_depth <- function(object, new_data, ...) {
  col_names <- colnames(object$data[[1]])
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  x_data <- as.matrix(new_data[, col_names])

  res <- lapply(
    object$data,
    get_depth,
    new_dat = x_data,
    metric = object$metric,
    opts = object$options
  )
  res <- tibble::new_tibble(res)

  new_names <- paste0(object$prefix, colnames(res))
  colnames(res) <- new_names

  res <- check_name(res, new_data, object, new_names)

  new_data <- vctrs::vec_cbind(new_data, res, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_depth <-
  function(x, width = max(20, options()$width - 30), ...) {
    if (x$trained) {
      title <- glue("Data depth by {x$class} for ")
      x_names <- colnames(x$data[[1]])
    } else {
      class <- rlang::quo_name(x$class[[1]])
      title <- glue("Data depth by {class} for ")
      x_names <- character()
    }

    print_step(x_names, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_depth <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = colnames(x$data[[1]]) %||% character(),
      class = x$class
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      class = na_chr
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_depth <- function(x, ...) {
  c("ddalpha")
}
