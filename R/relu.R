#' Apply (smoothed) rectified linear transformation
#'
#' \code{step_relu} creates a \emph{specification} of a recipe step that
#'   will apply the rectified linear or softplus transformations to numeric
#'   data.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See \code{\link{selections}} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param shift A numeric value dictating a translation to apply to the data.
#' @param reverse A logical to indicate if the left hinge should be used as
#'   opposed to the right hinge.
#' @param smooth A logical indicating if the softplus function, a smooth
#'   appromixation to the rectified linear transformation, should be used.
#'
#' @return An updated version of \code{recipe} with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @details The rectified linear transformation is calculated as
#'   \deqn{max(0, x - c)} and is also known as the ReLu or right hinge function.
#'   If \code{reverse} is true, then the transformation is reflected about the
#'   y-axis, like so: \deqn{max(0, c - x)} Setting the \code{smooth} option
#'   to true will instead calculate a smooth approximation to ReLu
#'   according to \deqn{ln(1 + e^(x - c)} The \code{reverse} argument may
#'   also be applied to this transformation.
#'
#' @section Connection to MARS
#'
#' The rectified linear transformation is used in the Multivariate Adaptive
#' Regression Splines as basis function to fit piecewise linear functions to
#' data in a strategy similar to that employeed in tree based models. The
#' transformation is a population choice as an activation function in many
#' neural networks, which could then seen as a stacked generalization of MARS
#' when making use of ReLu activations.
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
#'   prepare(training = biomass_tr) %>%
#'   bake(biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' @seealso \code{\link{recipe}} \code{\link{prepare.recipe}}
#'   \code{\link{bake.recipe}}
step_relu <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           shift = 0,
           reverse = FALSE,
           smooth = FALSE) {
    if (!is.numeric(shift))
      stop("Shift argument must be a numeric value.", call. = FALSE)
    if (!is.logical(reverse))
      stop("Reverse argument must be a logical value.", call. = FALSE)
    if (!is.logical(smooth))
      stop("Smooth argument must be logical value.", call. = FALSE)
    add_step(
      recipe,
      step_relu_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        shift = shift,
        reverse = reverse,
        smooth = smooth
      )
    )
  }

step_relu_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           shift = 0,
           reverse = FALSE,
           smooth = FALSE) {
    step(
      subclass = "relu",
      terms = terms,
      role = role,
      trained = trained,
      shift = shift,
      reverse = reverse,
      smooth = smooth
    )
  }

prepare.step_relu <- function(x, training, info = NULL, ...) {
  x$trained <- TRUE
  x
}

#' @importFrom dplyr mutate_at
bake.step_relu <- function(object, newdata, ...) {
  dplyr::mutate_at(
    newdata,
    vars(!!!object$terms),
    relu,
    object$shift,
    object$reverse,
    object$smooth
  )
}

relu <- function(x, shift = 0, reverse = FALSE, smooth = FALSE) {
  if (!is.numeric(x))
    stop("step_relu can only be applied to numeric data.", call. = FALSE)

  if (reverse) shifted <- shift - x
  else shifted <- x - shift

  if (smooth) out <- log1p(exp(shifted))  # use log1p for numerical accuracy
  else out <- pmax(shifted, rep(0, length(shifted)))
  out
}
