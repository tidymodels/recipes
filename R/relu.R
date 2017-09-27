#' Apply (Smoothed) Rectified Linear Transformation
#'
#' `step_relu` creates a *specification* of a recipe step that
#'   will apply the rectified linear or softplus transformations to numeric
#'   data. The transformed data is added as new columns to the data matrix.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param shift A numeric value dictating a translation to apply to the data.
#' @param reverse A logical to indicate if the left hinge should be used as
#'   opposed to the right hinge.
#' @param smooth A logical indicating if the softplus function, a smooth
#'   appromixation to the rectified linear transformation, should be used.
#' @param prefix A prefix for generated column names, default to "right_relu_"
#'   when right hinge transformation and "left_relu_" for reversed/left hinge
#'   transformations.
#'
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @details The rectified linear transformation is calculated as
#'   \deqn{max(0, x - c)} and is also known as the ReLu or right hinge function.
#'   If `reverse` is true, then the transformation is reflected about the
#'   y-axis, like so: \deqn{max(0, c - x)} Setting the `smooth` option
#'   to true will instead calculate a smooth approximation to ReLu
#'   according to \deqn{ln(1 + e^(x - c)} The `reverse` argument may
#'   also be applied to this transformation.
#'
#' @section Connection to MARS:
#'
#' The rectified linear transformation is used in the Multivariate Adaptive
#' Regression Splines as a basis function to fit piecewise linear functions to
#' data in a strategy similar to that employeed in tree based models. The
#' transformation is a popular choice as an activation function in many
#' neural networks, which could then be seen as a stacked generalization of
#' MARS when making use of ReLu activations. The hinge function also appears
#' in the loss function of Support Vector Machines, where it penalizes
#' residuals only if they are within a certain margin of the decision boundary.
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' transformed_te <- rec %>%
#'   step_relu(carbon, shift = 40) %>%
#'   prep(biomass_tr) %>%
#'   bake(biomass_te)
#'
#' transformed_te
#'
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_relu <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           shift = 0,
           reverse = FALSE,
           smooth = FALSE,
           prefix = "right_relu_") {
    if (!is.numeric(shift))
      stop("Shift argument must be a numeric value.", call. = FALSE)
    if (!is.logical(reverse))
      stop("Reverse argument must be a logical value.", call. = FALSE)
    if (!is.logical(smooth))
      stop("Smooth argument must be logical value.", call. = FALSE)
    if (reverse & prefix == "right_relu_")
      prefix <- "left_relu_"
    add_step(
      recipe,
      step_relu_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        shift = shift,
        reverse = reverse,
        smooth = smooth,
        prefix = prefix
      )
    )
  }

step_relu_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           shift = 0,
           reverse = FALSE,
           smooth = FALSE,
           prefix = "right_relu_") {
    step(
      subclass = "relu",
      terms = terms,
      role = role,
      trained = trained,
      shift = shift,
      reverse = reverse,
      smooth = smooth,
      prefix = prefix
    )
  }

prep.step_relu <- function(x, training, info = NULL, ...) {
  x$trained <- TRUE
  x
}

#' @importFrom dplyr select_vars tbl_vars
#' @importFrom rlang lang sym
bake.step_relu <- function(object, newdata, ...) {
  col_names <- select_vars(tbl_vars(newdata), !!!object$terms)

  make_relu_call <- function(col) {
    lang("relu", sym(col), object$shift, object$reverse, object$smooth)
  }

  names(col_names) <- paste0(object$prefix, col_names)
  exprs <- purrr::map(col_names, make_relu_call)

  dplyr::mutate(newdata, !!!exprs)
}


relu <- function(x, shift = 0, reverse = FALSE, smooth = FALSE) {
  if (!is.numeric(x))
    stop("step_relu can only be applied to numeric data.", call. = FALSE)

  if (reverse) {
    shifted <- shift - x
  } else {
    shifted <- x - shift
  }

  if (smooth) {
    out <- log1p(exp(shifted))  # use log1p for numerical accuracy
  } else {
    out <- pmax(shifted, rep(0, length(shifted)))
  }
  out
}
