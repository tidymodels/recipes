#' Generalized bernstein polynomial basis
#'
#' `step_poly_bernstein()` creates a *specification* of a recipe step that
#' creates Bernstein polynomial features.
#'
#' @inheritParams step_spline_b
#' @param degree The degrees of the polynomial. As the degrees for a polynomial
#'   increase, more flexible and complex curves can be generated.
#' @param options A list of options for [splines2::bernsteinPoly()] which should
#'   not include `x` or `degree`.
#' @template step-return
#' @export
#' @details
#'
#' Polynomial transformations take a numeric column and create multiple features
#' that, when used in a model, can estimate nonlinear trends between the column
#' and some outcome. The degrees of freedom determines how many new features are
#' added to the data.
#'
#' If the spline expansion fails for a selected column, the step will remove
#' that column's results (but will retain the original data). Use the `tidy()`
#' method to determine which columns were used.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_poly_bernstein"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2"))
#' library(tidyr)
#' library(dplyr)
#'
#' library(ggplot2)
#' data(ames, package = "modeldata")
#'
#' spline_rec <- recipe(Sale_Price ~ Longitude, data = ames) |>
#'   step_poly_bernstein(Longitude, degree = 6, keep_original_cols = TRUE) |>
#'   prep()
#'
#' tidy(spline_rec, number = 1)
#'
#' # Show where each feature is active
#' spline_rec |>
#'   bake(new_data =  NULL,-Sale_Price) |>
#'   pivot_longer(c(starts_with("Longitude_")), names_to = "feature", values_to = "value") |>
#'   mutate(feature = gsub("Longitude_", "feature ", feature)) |>
#'   filter(value > 0) |>
#'   ggplot(aes(x = Longitude, y = value)) +
#'   geom_line() +
#'   facet_wrap(~ feature)
#' @template case-weights-not-supported
#' @seealso [splines2::bernsteinPoly()]
step_poly_bernstein <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    degree = 10,
    complete_set = FALSE,
    options = NULL,
    keep_original_cols = FALSE,
    results = NULL,
    skip = FALSE,
    id = rand_id("poly_bernstein")
  ) {
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
  function(
    terms,
    trained,
    role,
    degree,
    complete_set,
    options,
    keep_original_cols,
    results,
    na_rm,
    skip,
    id
  ) {
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
#' @export
prep.step_poly_bernstein <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$complete_set, arg = "complete_set")
  check_number_whole(x$degree, arg = "degree", min = 0)
  check_options(x$options, exclude = c("x", 'degree'))

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

#' @export
bake.step_poly_bernstein <- function(object, new_data, ...) {
  col_names <- names(object$results)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  new_cols <- list()

  for (col_name in col_names) {
    new_cols[[col_name]] <- spline2_apply(
      object$results[[col_name]],
      new_data[[col_name]]
    )
  }

  new_cols <- purrr::list_cbind(unname(new_cols))
  new_cols <- check_name(new_cols, new_data, object, names(new_cols))

  new_data <- vec_cbind(new_data, new_cols, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

# ------------------------------------------------------------------------------

#' @export
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
