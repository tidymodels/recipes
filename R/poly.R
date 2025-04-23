#' Orthogonal polynomial basis functions
#'
#' `step_poly()` creates a *specification* of a recipe step that will create new
#' columns that are basis expansions of variables using orthogonal polynomials.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param objects A list of [stats::poly()] objects created once the step has
#'   been trained.
#' @param degree The polynomial degree (an integer).
#' @param options A list of options for [stats::poly()] which should not include
#'   `x`, `degree`, or `simple`. Note that the option `raw = TRUE` will produce
#'   the regular polynomial values (not orthogonalized).
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' `step_poly()` can create new features from a single variable that enable
#' fitting routines to model this variable in a nonlinear manner. The extent of
#' the possible nonlinearity is determined by the `degree` argument of
#' [stats::poly()]. The original variables are removed from the data by default,
#' but can be retained by setting `keep_original_cols = TRUE` and new columns
#' are added. The naming convention for the new variables is `varname_poly_1`
#' and so on.
#'
#' The orthogonal polynomial expansion is used by default because it yields
#' variables that are uncorrelated and doesn't produce large values which would
#' otherwise be a problem for large values of `degree`. Orthogonal polynomial
#' expansion pick up the same signal as their uncorrelated counterpart.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `degree` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{degree}{integer, the polynomial degree}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_poly"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
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
#' quadratic <- rec |>
#'   step_poly(carbon, hydrogen)
#' quadratic <- prep(quadratic, training = biomass_tr)
#'
#' expanded <- bake(quadratic, biomass_te)
#' expanded
#'
#' tidy(quadratic, number = 1)
step_poly <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    objects = NULL,
    degree = 2L,
    options = list(),
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("poly")
  ) {
    if (any(names(options) == "degree")) {
      degree <- options$degree
      cli::cli_inform(
        "The {.arg degree} argument is now a main argument instead of being \\
        within {.arg options}."
      )
    }

    add_step(
      recipe,
      step_poly_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        objects = objects,
        degree = degree,
        options = options,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_poly_new <-
  function(
    terms,
    role,
    trained,
    objects,
    degree,
    options,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "poly",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      degree = degree,
      options = options,
      keep_original_cols = keep_original_cols,
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
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_whole(x$degree, arg = "degree", min = 1)
  check_options(x$options, exclude = c("x", "simple"))

  x$degree <- as.integer(x$degree)

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
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_poly <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)
  new_names <- purrr::map(
    object$objects,
    ~ paste(attr(.x, "var"), "poly", seq_len(ncol(.x)), sep = "_")
  )

  # Start with n-row, 0-col tibble for the empty selection case
  new_tbl <- tibble::new_tibble(x = list(), nrow = nrow(new_data))

  for (col_name in col_names) {
    i_col <- new_data[[col_name]]
    i_object <- object$objects[[col_name]]
    i_new_names <- new_names[[col_name]]

    new_cols <- predict(i_object, i_col)
    colnames(new_cols) <- i_new_names
    new_cols <- tibble::as_tibble(new_cols)

    new_tbl[i_new_names] <- new_cols
  }

  new_tbl <- check_name(new_tbl, new_data, object, names(new_tbl))
  new_data <- vec_cbind(new_data, new_tbl, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_poly <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Polynomial expansion on "
    print_step(names(x$objects), x$terms, x$trained, title, width)
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
