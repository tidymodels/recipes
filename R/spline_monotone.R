#' Monotone splines
#'
#' `step_spline_monotone()` creates a *specification* of a recipe step that
#' creates monotone spline features.
#'
#' @inheritParams step_spline_b
#' @param degree The degree of I-spline defined to be the degree of the
#'   associated M-spline instead of actual polynomial degree. For example,
#'   I-spline basis of degree 2 is defined as the integral of associated
#'   M-spline basis of degree 2.
#' @param options A list of options for [splines2::iSpline()] which should not
#'   include `x`, `df`, `degree`, `periodic`, or `intercept`.
#' @return An object with classes `"step_spline_monotone"` and `"step"`.
#' @export
#' @details
#'
#' Spline transformations take a numeric column and create multiple features
#' that, when used in a model, can estimate nonlinear trends between the column
#' and some outcome. The degrees of freedom determines how many new features are
#' added to the data.
#'
#' These splines are integrated forms of M-splines and are non-negative and
#' monotonic. This means that, when used with a fitting function that produces
#' non-negative regression coefficients, the resulting fit is monotonic.
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
#' step <- "step_spline_monotone"
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
#'   step_spline_monotone(Longitude, deg_free = 6, keep_original_cols = TRUE) |>
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
#' @seealso [splines2::iSpline()]
step_spline_monotone <-
  function(
    recipe,
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
    id = rand_id("spline_monotone")
  ) {
    recipes_pkg_check(required_pkgs.step_spline_monotone())

    add_step(
      recipe,
      step_spline_monotone_new(
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

step_spline_monotone_new <-
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
      subclass = "spline_monotone",
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
prep.step_spline_monotone <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_bool(x$complete_set, arg = "complete_set")
  check_number_whole(x$degree, arg = "degree", min = 0)
  check_number_whole(x$deg_free, arg = "deg_free", min = 0)
  check_options(
    x$options,
    exclude = c("x", "df", "degree", "intercept")
  )

  check_zv(training[, col_names])

  res <- list()

  for (col_name in col_names) {
    res[[col_name]] <- spline2_create(
      training[[col_name]],
      nm = col_name,
      .fn = "iSpline",
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

  step_spline_monotone_new(
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
bake.step_spline_monotone <- function(object, new_data, ...) {
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
print.step_spline_monotone <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Monotone spline expansion "
    cols_used <- names(x$results)
    if (length(cols_used) == 0) {
      cols_used <- "<none>"
    }
    print_step(cols_used, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_spline_monotone <- function(x, ...) {
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
required_pkgs.step_spline_monotone <- function(x, ...) {
  c("splines2")
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_spline_monotone <- function(x, ...) {
  tibble::tibble(
    name = c("deg_free", "degree"),
    call_info = list(
      list(pkg = "dials", fun = "spline_degree", range = c(2L, 15L)),
      list(pkg = "dials", fun = "degree_int", range = c(0L, 4L))
    ),
    source = "recipe",
    component = "step_spline_monotone",
    component_id = x$id
  )
}
