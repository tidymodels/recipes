#' Orthogonal Polynomial Basis Functions
#'
#' `step_poly` creates a *specification* of a recipe
#'  step that will create new columns that are basis expansions of
#'  variables using orthogonal polynomials.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param objects A list of [stats::poly()] objects
#'  created once the step has been trained.
#' @param degree The polynomial degree (an integer).
#' @param options A list of options for [stats::poly()]
#'  which should not include `x`, `degree`, or `simple`. Note that
#'  the option `raw = TRUE` will produce the regular polynomial
#'  values (not orthogonalized).
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details `step_poly` can create new features from a single
#'  variable that enable fitting routines to model this variable in
#'  a nonlinear manner. The extent of the possible nonlinearity is
#'  determined by the `degree` argument of
#'  [stats::poly()]. The original variables are removed
#'  from the data and new columns are added. The naming convention
#'  for the new variables is `varname_poly_1` and so on.
#'
#'  When you [`tidy()`] this step, a tibble with columns `terms` (the
#'  columns that will be affected) and `degree` is returned.
#'
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
#' quadratic <- rec %>%
#'   step_poly(carbon, hydrogen)
#' quadratic <- prep(quadratic, training = biomass_tr)
#'
#' expanded <- bake(quadratic, biomass_te)
#' expanded
#'
#' tidy(quadratic, number = 1)
step_poly <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           objects = NULL,
           degree = 2,
           options = list(),
        skip = FALSE,
        id = rand_id("poly")) {

    if (!is_tune(degree) & !is_varying(degree)) {
      degree <- as.integer(degree)
    }

    if (any(names(options) == "degree")) {
      degree <- options$degree
      message(
        paste(
          "The `degree` argument is now a main argument instead of being",
          "within `options`."
        )
      )
    }

    add_step(
      recipe,
      step_poly_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        degree = degree,
        options = options,
        skip = skip,
        id = id
      )
    )
  }

step_poly_new <-
  function(terms, role, trained, objects, degree, options, skip, id) {
    step(
      subclass = "poly",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      degree = degree,
      options = options,
      skip = skip,
      id = id
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


#' @export
prep.step_poly <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  opts <- x$options
  opts$degree <- x$degree
  obj <- lapply(training[, col_names], poly_wrapper, opts)
  for (i in seq(along.with = col_names)) {
    attr(obj[[i]], "var") <- col_names[i]
  }

  step_poly_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = obj,
    degree = x$degree,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_poly <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  new_names <- purrr::map(object$objects, ~ paste(attr(.x, "var"), "poly", 1:ncol(.x), sep = "_"))
  poly_values <-
    purrr::map2(new_data[, col_names], object$objects, ~ predict(.y, .x)) %>%
    purrr::map(as_tibble) %>%
    purrr::map2_dfc(new_names, ~ setNames(.x, .y))
  new_data <- dplyr::bind_cols(new_data, poly_values)
  new_data <- dplyr::select(new_data, -dplyr::all_of(col_names))
  new_data
}


print.step_poly <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Orthogonal polynomials on ")
    printer(names(x$objects), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_poly <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$objects), degree = x$degree)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, degree = x$degree)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.recipe
#' @export
tunable.step_poly <- function(x, ...) {
  tibble::tibble(
    name = c("degree"),
    call_info = list(
      list(pkg = "dials", fun = "degree_int")
    ),
    source = "recipe",
    component = "step_poly",
    component_id = x$id
  )
}
