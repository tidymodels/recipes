#' Basis splines
#'
#' `step_spline_b()` creates a *specification* of a recipe step that creates
#' b-spline features.
#'
#' @inheritParams step_center
#' @param deg_free The degrees of freedom for the b-spline. As the degrees of
#'   freedom for a b-spline increase, more flexible and complex curves can be
#'   generated.
#' @param degree A non-negative integer specifying the degree of the piece-wise
#'   polynomial. The default value is 3 for cubic splines. Zero degree is
#'   allowed for piece-wise constant basis functions.
#' @param complete_set If `TRUE`, the complete basis matrix will be returned.
#'   Otherwise, the first basis will be excluded from the output. This maps to
#'   the `intercept` argument of the corresponding function from the
#'   \pkg{splines2} package and has the same default value.
#' @param results A list of objects created once the step has been trained.
#' @param options A list of options for [splines2::bSpline()] which should not
#'   include `x`, `df`, `degree`, or `intercept`.
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `FALSE`.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the new columns created by this step from the
#'   original variables will be used as _predictors_ in a model.
#' @return An object with classes `"step_spline_b"` and `"step"`.
#' @export
#' @details
#'
#' Spline transformations take a numeric column and create multiple features
#' that, when used in a model, can estimate nonlinear trends between the column
#' and some outcome. The degrees of freedom determines how many new features are
#' added to the data.
#'
#' Setting `periodic = TRUE` in the list passed to `options`, a periodic version
#' of the spline is used.
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
#' step <- "step_spline_b"
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
#'   step_spline_b(Longitude, deg_free = 6, keep_original_cols = TRUE) |>
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
#' @seealso [splines2::bSpline()]
step_spline_b <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    deg_free = 10,
    degree = 3,
    complete_set = FALSE,
    options = NULL,
    keep_original_cols = FALSE,
    results = NULL,
    skip = FALSE,
    id = rand_id("spline_b")
  ) {
    recipes_pkg_check(required_pkgs.step_spline_b())

    add_step(
      recipe,
      step_spline_b_new(
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

step_spline_b_new <-
  function(
    terms,
    trained,
    role,
    deg_free,
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
      subclass = "spline_b",
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
#' @export
prep.step_spline_b <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$complete_set, arg = "complete_set")
  check_number_whole(x$degree, arg = "degree", min = 0)
  check_number_whole(x$deg_free, arg = "deg_free", min = 0)
  check_options(x$options, exclude = c("x", "df", "degree", "intercept"))

  check_zv(training[, col_names])

  res <- list()

  for (col_name in col_names) {
    res[[col_name]] <- spline2_create(
      training[[col_name]],
      nm = col_name,
      .fn = "bSpline",
      df = x$deg_free,
      complete_set = x$complete_set,
      degree = x$degree,
      fn_opts = x$options
    )
  }
  # check for errors
  bas_res <- purrr::map_lgl(res, is.null)
  res <- res[!bas_res]
  col_names <- col_names[!bas_res]
  names(res) <- col_names

  step_spline_b_new(
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

#' @export
bake.step_spline_b <- function(object, new_data, ...) {
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
print.step_spline_b <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Basis spline expansion "
    cols_used <- names(x$results)
    if (length(cols_used) == 0) {
      cols_used <- "<none>"
    }
    print_step(cols_used, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_spline_b <- function(x, ...) {
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
required_pkgs.step_spline_b <- function(x, ...) {
  c("splines2")
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_spline_b <- function(x, ...) {
  tibble::tibble(
    name = c("deg_free", "degree"),
    call_info = list(
      list(pkg = "dials", fun = "spline_degree", range = c(2L, 15L)),
      list(pkg = "dials", fun = "degree_int", range = c(1L, 4L))
    ),
    source = "recipe",
    component = "step_spline_b",
    component_id = x$id
  )
}
