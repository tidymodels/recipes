#' Generalized Bernstein Polynomial Basis
#'
#' `step_poly_bernstein` creates a *specification* of a recipe
#'  step that creates Bernstein polynomial features.
#'
#' @inheritParams step_spline_b
#' @param degree The degrees of the polynomial. As the degrees for a polynomial
#'  increase, more flexible and complex curves can be generated.
#' @param options A list of options for [splines2::bernsteinPoly()]
#'  which should not include `x` or `degree`.
#' @return An object with classes `"step_poly_bernstein"` and `"step"`.
#' @export
#' @details
#'
#' Polynomial transformations take a numeric column and create multiple features
#' that, when used in a model, can estimate nonlinear trends between the column
#' and some outcome. The degrees of freedom determines how many new features
#' are added to the data.
#'
#' If the spline expansion fails for a selected column, the step will
#' remove that column's results (but will retain the original data). Use the
#' `tidy()` method to determine which columns were used.
#'
#' # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#'  `terms` (the columns that will be affected) is returned.
#'
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2"))
#' library(tidyr)
#' library(dplyr)
#'
#' library(ggplot2)
#' data(ames, package = "modeldata")
#'
#' spline_rec <- recipe(Sale_Price ~ Longitude, data = ames) %>%
#'   step_poly_bernstein(Longitude, degree = 6, keep_original_cols = TRUE) %>%
#'   prep()
#'
#' tidy(spline_rec, number = 1)
#'
#' # Show where each feature is active
#' spline_rec %>%
#'   bake(new_data =  NULL,-Sale_Price) %>%
#'   pivot_longer(c(starts_with("Longitude_")), names_to = "feature", values_to = "value") %>%
#'   mutate(feature = gsub("Longitude_", "feature ", feature)) %>%
#'   filter(value > 0) %>%
#'   ggplot(aes(x = Longitude, y = value)) +
#'   geom_line() +
#'   facet_wrap(~ feature)
#' @template case-weights-not-supported
#' @seealso [splines2::bernsteinPoly()]
step_poly_bernstein <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           degree = 10,
           complete_set = FALSE,
           options = NULL,
           keep_original_cols = FALSE,
           results = NULL,
           skip = FALSE,
           id = rand_id("poly_bernstein")) {

    recipes_pkg_check(required_pkgs.step_poly_bernstein())

    add_step(
      recipe,
      step_poly_bernstein_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        degree = degree,
        complete_set = complete_set,
        options = options,
        keep_original_cols = keep_original_cols,
        results = results,
        skip = skip,
        id = id
      )
    )
  }

step_poly_bernstein_new <-
  function(terms, trained, role, degree, complete_set, options, keep_original_cols,
           results, na_rm, skip, id) {
    step(
      subclass = "poly_bernstein",
      terms = terms,
      role = role,
      trained = trained,
      degree = degree,
      complete_set = complete_set,
      options = options,
      keep_original_cols = keep_original_cols,
      results = results,
      skip = skip,
      id = id
    )
  }

# ------------------------------------------------------------------------------

prep.step_poly_bernstein <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)

  x$options <- c(x$options, degree = x$degree)

  res <-
    purrr::map2(
      training[, col_names],
      col_names,
      ~ spline2_create(
        .x,
        nm = .y,
        .fn = "bernsteinPoly",
        df = NULL,
        complete_set = x$complete_set,
        fn_opts = x$options
      )
    )
  # check for errors
  bas_res <- purrr::map_lgl(res, is.null)
  res <- res[!bas_res]
  col_names <- col_names[!bas_res]
  names(res) <- col_names

  step_poly_bernstein_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    degree = x$degree,
    complete_set = x$complete_set,
    options = x$options,
    keep_original_cols = x$keep_original_cols,
    results = res,
    skip = x$skip,
    id = x$id
  )
}

bake.step_poly_bernstein <- function(object, new_data, ...) {
  orig_names <- names(object$results)
  if (length(orig_names) > 0) {
    new_cols <- purrr::map2_dfc(object$results, new_data[, orig_names], spline2_apply)
    new_data <- bind_cols(new_data, new_cols)
    keep_original_cols <- get_keep_original_cols(object)
    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% orig_names), drop = FALSE]
    }
  }
  as_tibble(new_data)
}

# ------------------------------------------------------------------------------

print.step_poly_bernstein <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Bernstein polynomial expansion "
    cols_used <- names(x$results)
    if (length(cols_used) == 0) {
      cols_used <- "<none>"
    }
    print_step(cols_used, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_poly_bernstein <- function(x, ...) {
  if (is_trained(x)) {
    terms <- names(x$results)
    if (length(terms) == 0) {
      terms <- "<none>"
    }
  } else {
    terms <- sel2char(x$terms)
  }
  tibble(terms = terms, id = x$id)
}

# ------------------------------------------------------------------------------

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_poly_bernstein <- function(x, ...) {
  c("splines2")
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_poly_bernstein <- function(x, ...) {
  tibble::tibble(
    name = c("degree"),
    call_info = list(
      list(pkg = "dials", fun = "degree_int", range = c(1L, 15L))
    ),
    source = "recipe",
    component = "step_poly_bernstein",
    component_id = x$id
  )
}
