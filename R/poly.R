#' Orthogonal Polynomial Basis Functions
#'
#' `step_poly` creates a *specification* of a recipe
#'  step that will create new columns that are basis expansions of
#'  variables using orthogonal polynomials.

#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created from the original variables will be
#'  used as predictors in a model.
#' @param objects A list of [stats::poly()] objects
#'  created once the step has been trained.
#' @param options A list of options for [stats::poly()]
#'  which should not include `x` or `simple`. Note that
#'  the option `raw = TRUE` will produce the regular polynomial
#'  values (not orthogonalized).
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `degree`.
#' @keywords datagen
#' @concept preprocessing basis_expansion
#' @export
#' @details `step_poly` can new features from a single
#'  variable that enable fitting routines to model this variable in
#'  a nonlinear manner. The extent of the possible nonlinearity is
#'  determined by the `degree` argument of
#'  [stats::poly()]. The original variables are removed
#'  from the data and new columns are added. The naming convention
#'  for the new variables is `varname_poly_1` and so on.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' quadratic <- rec %>%
#'   step_poly(carbon, hydrogen)
#' quadratic <- prep(quadratic, training = biomass_tr)
#'
#' expanded <- bake(quadratic, biomass_te)
#' expanded
#'
#' tidy(quadratic, number = 1)
#' @seealso [step_ns()] [recipe()]
#'   [prep.recipe()] [bake.recipe()]


step_poly <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           objects = NULL,
           options = list(degree = 2)) {
    add_step(
      recipe,
      step_poly_new(
        terms = check_ellipses(...),
        trained = trained,
        role = role,
        objects = objects,
        options = options
      )
    )
  }

step_poly_new <- function(terms = NULL,
                          role = NA,
                          trained = FALSE,
                          objects = NULL,
                          options = NULL) {
  step(
    subclass = "poly",
    terms = terms,
    role = role,
    trained = trained,
    objects = objects,
    options = options
  )
}


poly_wrapper <- function(x, args) {
  args$x <- x
  args$simple <- FALSE
  poly_obj <- do.call("poly", args)

  ## don't need to save the original data so keep 1 row
  out <- matrix(NA, ncol = ncol(poly_obj), nrow = 1)
  class(out) <- c("poly", "basis", "matrix")
  attr(out, "degree") <- attr(poly_obj, "degree")
  attr(out, "coefs") <- attr(poly_obj, "coefs")
  out
}

#' @importFrom stats poly
#' @export
prep.step_poly <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  obj <- lapply(training[, col_names], poly_wrapper, x$options)
  for (i in seq(along = col_names))
    attr(obj[[i]], "var") <- col_names[i]

  step_poly_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = obj,
    options = x$options
  )
}

#' @importFrom tibble as_tibble is_tibble
#' @importFrom stats predict
#' @export
bake.step_poly <- function(object, newdata, ...) {
  ## pre-allocate a matrix for the basis functions.
  new_cols <- vapply(object$objects, ncol, c(int = 1L))
  poly_values <-
    matrix(NA, nrow = nrow(newdata), ncol = sum(new_cols))
  colnames(poly_values) <- rep("", sum(new_cols))
  strt <- 1
  for (i in names(object$objects)) {
    cols <- (strt):(strt + new_cols[i] - 1)
    orig_var <- attr(object$objects[[i]], "var")
    poly_values[, cols] <-
      predict(object$objects[[i]], getElement(newdata, i))
    new_names <-
      paste(orig_var, "poly", names0(new_cols[i], ""), sep = "_")
    colnames(poly_values)[cols] <- new_names
    strt <- max(cols) + 1
    newdata[, orig_var] <- NULL
  }
  newdata <- cbind(newdata, as_tibble(poly_values))
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}


print.step_poly <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Orthogonal polynomials on ")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_poly
#' @param x A `step_poly` object.
tidy.step_poly <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$objects),
                  degree = x$options$degree)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  degree = x$options$degree)
  }
  res
}
