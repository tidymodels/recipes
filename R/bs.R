#' B-Spline Basis Functions
#'
#' `step_bs` creates a *specification* of a recipe step
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
#' @param deg_free The degrees of freedom for the spline. As the
#'  degrees of freedom for a spline increase, more flexible and
#'  complex curves can be generated. When a single degree of freedom is used,
#'  the result is a rescaled version of the original data.
#' @param degree Degree of polynomial spline (integer).
#' @param options A list of options for [splines::bs()]
#'  which should not include `x`, `degree`, or `df`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the columns that will be affected and `holiday`.
#' @keywords datagen
#' @concept preprocessing
#' @concept basis_expansion
#' @export
#' @details `step_bs` can create new features from a single variable
#'  that enable fitting routines to model this variable in a
#'  nonlinear manner. The extent of the possible nonlinearity is
#'  determined by the `df`, `degree`, or `knot` arguments of
#'  [splines::bs()]. The original variables are removed
#'  from the data and new columns are added. The naming convention
#'  for the new variables is `varname_bs_1` and so on.
#' @examples
#' library(modeldata)
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
           deg_free = NULL,
           degree = 3,
           objects = NULL,
           options = list(),
           skip = FALSE,
           id = rand_id("bs")) {

    add_step(
      recipe,
      step_bs_new(
        terms = ellipse_check(...),
        trained = trained,
        deg_free = deg_free,
        degree = degree,
        role = role,
        objects = objects,
        options = options,
        skip = skip,
        id = id
      )
    )
  }

step_bs_new <-
  function(terms, role, trained, deg_free, degree, objects, options, skip, id) {
    step(
      subclass = "bs",
      terms = terms,
      role = role,
      trained = trained,
      deg_free = deg_free,
      degree = degree,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  }

splines_wrapper <- function(x, args, type = c("bs", "ns")) {
  type <- match.arg(type)

  # Only do the parameter computations from splines::bs() / splines::ns(), don't evaluate at x.
  if (!"degree" %in% names(args)) {
    args$degree <- switch(type, bs = 3L, ns = 1L)
  } else {
    args$degree <- as.integer(args$degree)
    if (!args$degree >= 1L) {
      stop("B-Spline 'degree' must be integer >= 1")
    }
  }
  if (!"intercept" %in% names(args)) {
    args$intercept <- FALSE
  } else {
    args$intercept <- as.logical(args$intercept)
  }
  if (!("Boundary.knots" %in% names(args))) {
    args$Boundary.knots <- range(x)
    if (length(x) == 1L && type == "ns") {
      args$Boundary.knots <- x * c(7, 9) / 8
    }
  } else {
    args$Boundary.knots <- sort(args$Boundary.knots)
  }

  if (!is.null(args$df) && is.null(args$knots)) {
    ord <- 1L + args$degree
    ok <- !is.na(x) &
      x >= args$Boundary.knots[1L] & x <= args$Boundary.knots[2L]
    nIknots <- args$df - ord + (1L - args$intercept)
    if (nIknots < 0L) {
      nIknots <- 0L
      warning(gettextf("'df' was too small; have used %d",
                       ord - (1L - args$intercept)), domain = NA)
    }
    if (nIknots > 0L) {
      probs <- seq.int(from = 0, to = 1, length.out = nIknots +
        2L)[-c(1L, nIknots + 2L)]
      args$knots <- quantile(x[ok], probs)
    } else {
      args$knots <- numeric()
    }
  } else {
    args$knots <- numeric()
  }

  # Only construct the data necessary for splines_predict
  out <- matrix(NA, ncol = args$degree + length(args$knots) + args$intercept, nrow = 1)
  class(out) <- c(type, "basis", "matrix")
  attr(out, "knots") <- args$knots
  attr(out, "degree") <- args$degree
  attr(out, "Boundary.knots") <- args$Boundary.knots
  attr(out, "intercept") <- args$intercept
  out
}

splines_predict <- function(object, x) {
  xu <- unique(x)
  ru <- predict(object, xu)
  res <- ru[match(x, xu), ]
  copy_attrs <- c("class", "degree", "knots", "Boundary.knots", "intercept")
  attributes(res)[copy_attrs] <- attributes(ru)[copy_attrs]
  res
}

#' @export
prep.step_bs <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)
  check_type(training[, col_names])

  opt <- x$options
  opt$df <- x$deg_free
  opt$degree <- x$degree
  obj <- lapply(training[, col_names], splines_wrapper, opt, type = "bs")
  for (i in seq(along.with = col_names))
    attr(obj[[i]], "var") <- col_names[i]
  step_bs_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    deg_free = x$deg_free,
    degree = x$degree,
    objects = obj,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_bs <- function(object, new_data, ...) {
  ## pre-allocate a matrix for the basis functions.
  new_cols <- vapply(object$objects, ncol, c(int = 1L))
  bs_values <-
    matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))
  colnames(bs_values) <- rep("", sum(new_cols))
  strt <- 1
  for (i in names(object$objects)) {
    cols <- (strt):(strt + new_cols[i] - 1)
    orig_var <- attr(object$objects[[i]], "var")
    bs_values[, cols] <-
      splines_predict(object$objects[[i]], getElement(new_data, i))
    new_names <-
      paste(orig_var, "bs", names0(new_cols[i], ""), sep = "_")
    colnames(bs_values)[cols] <- new_names
    strt <- max(cols) + 1
    new_data[, orig_var] <- NULL
  }
  new_data <- bind_cols(new_data, as_tibble(bs_values))
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}


print.step_bs <-
  function(x, width = max(20, options()$width - 28), ...) {
    cat("B-Splines on ")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_bs
#' @param x A `step_bs` object.
#' @export
tidy.step_bs <- function(x, ...) {
  if (is_trained(x)) {
    cols <- tibble(terms = names(x$objects))
  } else {
    cols <- sel2char(x$terms)
  }
  res <- expand.grid(terms = cols, stringsAsFactors = FALSE)
  res$id <- x$id
  as_tibble(res)
}

# ------------------------------------------------------------------------------

#' @rdname tunable.step
#' @export
tunable.step_bs <- function(x, ...) {
  tibble::tibble(
    name = c("deg_free", "degree"),
    call_info = list(
      list(pkg = "dials", fun = "spline_degree", range = c(1L, 15L)),
      list(pkg = "dials", fun = "degree_int", range = c(1L, 2L))
    ),
    source = "recipe",
    component = "step_bs",
    component_id = x$id
  )
}
