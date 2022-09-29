#' Convex Splines
#'
#' `step_spline_convex` creates a *specification* of a recipe
#'  step that creates convex spline features.
#'
#' @inheritParams step_spline_b
#' @param degree The degree of C-spline defined to be the degree of the
#'  associated M-spline instead of actual polynomial degree. For example,
#'  C-spline basis of degree 2 is defined as the scaled double integral of
#'  associated M-spline basis of degree 2.
#' @param options A list of options for [splines2::cSpline()]
#'  which should not include `x`, `df`, `degree`, or `intercept`.
#' @return An object with classes `"step_spline_convex"` and `"step"`.
#' @export
#' @details
#'
#' Spline transformations take a numeric column and create multiple features
#' that, when used in a model, can estimate nonlinear trends between the column
#' and some outcome. The degrees of freedom determines how many new features
#' are added to the data.
#'
#' If a single degree of freedom is requested, two degrees of freedom will be
#' used.
#'
#' If the spline expansion fails for a selected column, the step will silently
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
#'   step_spline_convex(Longitude, deg_free = 6, keep_original_cols = TRUE) %>%
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
#' @seealso [splines2::cSpline()]
step_spline_convex <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           deg_free = 10,
           degree = 3,
           complete_set = TRUE,
           options = NULL,
           keep_original_cols = FALSE,
           results = NULL,
           skip = FALSE,
           id = rand_id("spline_convex")) {

    recipes_pkg_check(required_pkgs.step_spline_convex())

    add_step(
      recipe,
      step_spline_convex_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        deg_free = deg_free,
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

step_spline_convex_new <-
  function(terms, trained, role, deg_free, degree, complete_set, options,
           keep_original_cols, results, na_rm, skip, id) {
    step(
      subclass = "spline_convex",
      terms = terms,
      role = role,
      trained = trained,
      deg_free = deg_free,
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

prep.step_spline_convex <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], quant = TRUE)

  res <-
    purrr::map2(
      training[, col_names],
      col_names,
      ~ spline2_create(
        .x,
        nm = .y,
        .fn = "cSpline",
        df = x$deg_free,
        degree = x$degree,
        complete_set = x$complete_set,
        fn_opts = x$options
      )
    )
  # check for errors
  bas_res <- purrr::map_lgl(res, is.null)
  res <- res[!bas_res]
  col_names <- col_names[!bas_res]
  names(res) <- col_names

  step_spline_convex_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    deg_free = x$deg_free,
    degree = x$degree,
    complete_set = x$complete_set,
    options = x$options,
    keep_original_cols = x$keep_original_cols,
    results = res,
    skip = x$skip,
    id = x$id
  )
}

bake.step_spline_convex <- function(object, new_data, ...) {
  orig_names <- names(object$results)
  if (length(orig_names) > 0) {
    new_cols <- purrr::map2_dfc(object$results, new_data[, orig_names], spline2_apply)
    new_data <- bind_cols(new_data, new_cols)
    keep_original_cols <- get_keep_original_cols(object)
    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% orig_names), drop = FALSE]
    }
  }
  new_data
}

# ------------------------------------------------------------------------------

print.step_spline_convex <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Convex spline expansion "
    cols_used <- names(x$results)
    if (length(cols_used) == 0) {
      cols_used <- "<none>"
    }
    print_step(cols_used, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_spline_convex <- function(x, ...) {
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
required_pkgs.step_spline_convex <- function(x, ...) {
  c("splines2")
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_spline_convex <- function(x, ...) {
  tibble::tibble(
    name = c("deg_free"),
    call_info = list(
      list(pkg = "dials", fun = "spline_degree", range = c(2L, 15L))
    ),
    source = "recipe",
    component = "step_spline_convex",
    component_id = x$id
  )
}