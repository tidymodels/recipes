#' B-Spline Basis Functions
#'
#' `step_ns` creates a *specification* of a recipe step
#'  that will create new columns that are basis expansions of
#'  variables using B-splines.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created from the original variables will be
#'  used as predictors in a model.
#' @param objects A list of [splines::bs()] objects
#'  created once the step has been trained.
#' @param options A list of options for [splines::bs()]
#'  which should not include `x`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the columns that will be affected and `holiday`.
#' @keywords datagen
#' @concept preprocessing basis_expansion
#' @export
#' @details `step_bs` can new features from a single variable
#'  that enable fitting routines to model this variable in a
#'  nonlinear manner. The extent of the possible nonlinearity is
#'  determined by the `df`, `degree`, or `knot` arguments of
#'  [splines::bs()]. The original variables are removed
#'  from the data and new columns are added. The naming convention
#'  for the new variables is `varname_bs_1` and so on.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' with_splines <- rec %>%
#'   step_bs(carbon, hydrogen)
#' with_splines <- prep(with_splines, training = biomass_tr)
#'
#' expanded <- bake(with_splines, biomass_te)
#' expanded
#' @seealso [step_poly()] [recipe()] [step_ns()]
#'   [prep.recipe()] [bake.recipe()]

step_bs <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           objects = NULL,
           options = list(df = NULL, degree = 3),
           skip = FALSE) {
    add_step(
      recipe,
      step_bs_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        options = options,
        skip = skip
      )
    )
  }

step_bs_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           objects = NULL,
           options = NULL,
           skip = FALSE) {
    step(
      subclass = "bs",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      options = options,
      skip = skip
    )
  }

#' @importFrom splines bs
bs_wrapper <- function(x, args) {
  if (!("Boundary.knots" %in% names(args)))
    args$Boundary.knots <- range(x)
  args$x <- x
  bs_obj <- do.call("bs", args)
  ## don't need to save the original data so keep 1 row
  out <- matrix(NA, ncol = ncol(bs_obj), nrow = 1)
  class(out) <- c("bs", "basis", "matrix")
  attr(out, "knots") <- attr(bs_obj, "knots")[]
  attr(out, "degree") <- attr(bs_obj, "degree")
  attr(out, "Boundary.knots") <- attr(bs_obj, "Boundary.knots")
  attr(out, "intercept") <- attr(bs_obj, "intercept")
  out
}

#' @export
prep.step_bs <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  obj <- lapply(training[, col_names], bs_wrapper, x$options)
  for (i in seq(along = col_names))
    attr(obj[[i]], "var") <- col_names[i]
  step_bs_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = obj,
    options = x$options,
    skip = x$skip
  )
}

#' @importFrom tibble as_tibble is_tibble
#' @importFrom stats predict
#' @export
bake.step_bs <- function(object, newdata, ...) {
  ## pre-allocate a matrix for the basis functions.
  new_cols <- vapply(object$objects, ncol, c(int = 1L))
  bs_values <-
    matrix(NA, nrow = nrow(newdata), ncol = sum(new_cols))
  colnames(bs_values) <- rep("", sum(new_cols))
  strt <- 1
  for (i in names(object$objects)) {
    cols <- (strt):(strt + new_cols[i] - 1)
    orig_var <- attr(object$objects[[i]], "var")
    bs_values[, cols] <-
      predict(object$objects[[i]], getElement(newdata, i))
    new_names <-
      paste(orig_var, "bs", names0(new_cols[i], ""), sep = "_")
    colnames(bs_values)[cols] <- new_names
    strt <- max(cols) + 1
    newdata[, orig_var] <- NULL
  }
  newdata <- bind_cols(newdata, as_tibble(bs_values))
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}


print.step_bs <-
  function(x, width = max(20, options()$width - 28), ...) {
    cat("B-Splines on ")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_bs
#' @param x A `step_bs` object.
tidy.step_bs <- function(x, ...) {
  res <- simple_terms(x, ...)
  res <- expand.grid(terms = res$terms,
                     stringsAsFactors = FALSE)
  as_tibble(res)
}
