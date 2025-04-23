#' Apply (smoothed) rectified linear transformation
#'
#' `step_relu()` creates a *specification* of a recipe step that will add the
#' rectified linear or softplus transformations of a variable to the data set.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param shift A numeric value dictating a translation to apply to the data.
#' @param reverse A logical to indicate if the left hinge should be used as
#'   opposed to the right hinge.
#' @param smooth A logical indicating if the softplus function, a smooth
#'   approximation to the rectified linear transformation, should be used.
#' @param prefix A prefix for generated column names, defaults to "right_relu_"
#'   for right hinge transformation and "left_relu_" for reversed/left hinge
#'   transformations.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @rdname step_relu
#'
#' @details
#'
#' The rectified linear transformation is calculated as \deqn{max(0, x - c)} and
#' is also known as the ReLu or right hinge function. If `reverse` is true, then
#' the transformation is reflected about the y-axis, like so: \deqn{max(0, c -
#' x)} Setting the `smooth` option to true will instead calculate a smooth
#' approximation to ReLu according to \deqn{ln(1 + e^(x - c)} The `reverse`
#' argument may also be applied to this transformation.
#'
#' # Connection to MARS:
#'
#' The rectified linear transformation is used in Multivariate Adaptive
#' Regression Splines as a basis function to fit piecewise linear functions to
#' data in a strategy similar to that employed in tree based models. The
#' transformation is a popular choice as an activation function in many neural
#' networks, which could then be seen as a stacked generalization of MARS when
#' making use of ReLu activations. The hinge function also appears in the loss
#' function of Support Vector Machines, where it penalizes residuals only if
#' they are within a certain margin of the decision boundary.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `shift`, `reverse` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{shift}{numeric, location of hinge}
#'   \item{reverse}{logical, whether left hinge is used}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' transformed_te <- rec |>
#'   step_relu(carbon, shift = 40) |>
#'   prep(biomass_tr) |>
#'   bake(biomass_te)
#'
#' transformed_te
step_relu <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    shift = 0,
    reverse = FALSE,
    smooth = FALSE,
    prefix = "right_relu_",
    columns = NULL,
    skip = FALSE,
    id = rand_id("relu")
  ) {
    add_step(
      recipe,
      step_relu_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        shift = shift,
        reverse = reverse,
        smooth = smooth,
        prefix = prefix,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_relu_new <-
  function(
    terms,
    role,
    trained,
    shift,
    reverse,
    smooth,
    prefix,
    columns,
    skip,
    id
  ) {
    step(
      subclass = "relu",
      terms = terms,
      role = role,
      trained = trained,
      shift = shift,
      reverse = reverse,
      smooth = smooth,
      prefix = prefix,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_relu <- function(x, training, info = NULL, ...) {
  columns <- recipes_eval_select(x$terms, training, info)
  check_type(training[, columns], types = c("double", "integer"))
  check_number_decimal(x$shift, arg = "shift")
  check_bool(x$reverse, arg = "reverse")
  check_bool(x$smooth, arg = "smooth")
  if (x$reverse & x$prefix == "right_relu_") {
    x$prefix <- "left_relu_"
  }

  step_relu_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shift = x$shift,
    reverse = x$reverse,
    smooth = x$smooth,
    prefix = x$prefix,
    columns = columns,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_relu <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  make_relu_call <- function(col) {
    call2("relu", sym(col), object$shift, object$reverse, object$smooth)
  }

  exprs <- purrr::map(col_names, make_relu_call)
  newname <- glue::glue("{object$prefix}{col_names}")
  exprs <- check_name(exprs, new_data, object, newname, TRUE)
  dplyr::mutate(new_data, !!!exprs)
}

#' @export
print.step_relu <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Adding relu transform for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

relu <- function(x, shift = 0, reverse = FALSE, smooth = FALSE) {
  if (reverse) {
    shifted <- shift - x
  } else {
    shifted <- x - shift
  }

  if (smooth) {
    out <- log1p(exp(shifted)) # use log1p for numerical accuracy
  } else {
    out <- pmax(shifted, rep(0, length(shifted)))
  }
  out
}

#' @rdname tidy.recipe
#' @export
tidy.step_relu <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$shift <- x$shift
  out$reverse <- x$reverse
  out$id <- x$id
  out
}
