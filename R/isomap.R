#' Isomap Embedding
#'
#' `step_isomap` creates a *specification* of a recipe
#'  step that will convert numeric data into one or more new
#'  dimensions.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param num_terms The number of isomap dimensions to retain as new
#'  predictors. If `num_terms` is greater than the number of columns
#'  or the number of possible dimensions, a smaller value will be
#'  used.
#' @param neighbors The number of neighbors.
#' @param options A list of options to [dimRed::Isomap()].
#' @param res The [dimRed::Isomap()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep()].
#' @param columns A character string of variable names that will
#'  be populated elsewhere.
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details Isomap is a form of multidimensional scaling (MDS).
#'  MDS methods try to find a reduced set of dimensions such that
#'  the geometric distances between the original data points are
#'  preserved. This version of MDS uses nearest neighbors in the
#'  data as a method for increasing the fidelity of the new
#'  dimensions to the original data values.
#'
#' This step requires the \pkg{dimRed}, \pkg{RSpectra},
#'  \pkg{igraph}, and \pkg{RANN} packages. If not installed, the
#'  step will stop with a note about installing these packages.
#'
#'
#' It is advisable to center and scale the variables prior to
#'  running Isomap (`step_center` and `step_scale` can be
#'  used for this purpose).
#'
#' The argument `num_terms` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_terms < 10`, their names will be `Isomap1` -
#'  `Isomap9`. If `num_terms = 101`, the names would be
#'  `Isomap001` - `Isomap101`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#' `terms` (the selectors or variables selected) is returned.
#'
#' @template case-weights-not-supported
#'
#' @references De Silva, V., and Tenenbaum, J. B. (2003). Global
#'  versus local methods in nonlinear dimensionality reduction.
#'  *Advances in Neural Information Processing Systems*.
#'  721-728.
#'
#' \pkg{dimRed}, a framework for dimensionality reduction,
#'   https://github.com/gdkrmr
#'
#' @examplesIf rlang::is_installed("modeldata")
#' \donttest{
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
#' im_trans <- rec %>%
#'   step_YeoJohnson(all_numeric_predictors()) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_isomap(all_numeric_predictors(), neighbors = 100, num_terms = 2)
#'
#' if (FALSE) {
#'   im_estimates <- prep(im_trans, training = biomass_tr)
#'
#'   im_te <- bake(im_estimates, biomass_te)
#'
#'   rng <- extendrange(c(im_te$Isomap1, im_te$Isomap2))
#'   plot(im_te$Isomap1, im_te$Isomap2,
#'     xlim = rng, ylim = rng
#'   )
#'
#'   tidy(im_trans, number = 3)
#'   tidy(im_estimates, number = 3)
#' }
#' }
step_isomap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_terms = 5,
           neighbors = 50,
           options = list(.mute = c("message", "output")),
           res = NULL,
           columns = NULL,
           prefix = "Isomap",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("isomap")) {
    recipes_pkg_check(required_pkgs.step_isomap())

    add_step(
      recipe,
      step_isomap_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_terms = num_terms,
        neighbors = neighbors,
        options = options,
        res = res,
        columns = columns,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_isomap_new <-
  function(terms, role, trained, num_terms, neighbors, options, res, columns,
           prefix, keep_original_cols, skip, id) {
    step(
      subclass = "isomap",
      terms = terms,
      role = role,
      trained = trained,
      num_terms = num_terms,
      neighbors = neighbors,
      options = options,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_isomap <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  if (x$num_terms > 0 && length(col_names) > 0L) {
    x$num_terms <- min(x$num_terms, ncol(training))
    x$neighbors <- min(x$neighbors, nrow(training))

    iso_map <-
      try(
        eval_dimred_call(
          "embed",
          .data = dimred_data(training[, col_names, drop = FALSE]),
          .method = "Isomap",
          knn = x$neighbors,
          ndim = x$num_terms,
          .mute = x$options$.mute
        ),
        silent = TRUE
      )
    if (inherits(iso_map, "try-error")) {
      rlang::abort(paste0("`step_isomap` failed with error:\n", as.character(iso_map)))
    }
  } else {
    iso_map <- NULL
  }

  step_isomap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_terms = x$num_terms,
    neighbors = x$neighbors,
    options = x$options,
    res = iso_map,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_isomap <- function(object, new_data, ...) {
  check_new_data(names(object$columns), object, new_data)

  if (object$num_terms > 0 && length(object$columns) > 0L) {
    isomap_vars <- colnames(environment(object$res@apply)$indata)
    suppressMessages({
      comps <- object$res@apply(
        dimred_data(new_data[, isomap_vars, drop = FALSE])
      )@data
    })
    comps <- comps[, 1:object$num_terms, drop = FALSE]
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, as_tibble(comps))
    keep_original_cols <- get_keep_original_cols(object)
    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% isomap_vars), drop = FALSE]
    }
  }
  new_data
}


print.step_isomap <- function(x, width = max(20, options()$width - 35), ...) {
  if (x$num_terms == 0) {
    title <- "Isomap was not conducted for "
  } else {
    title <- "Isomap approximation with "
  }

  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}


#' @rdname tidy.recipe
#' @export
tidy.step_isomap <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_isomap <- function(x, ...) {
  tibble::tibble(
    name = c("num_terms", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "num_terms", range = c(1L, 4L)),
      list(pkg = "dials", fun = "neighbors", range = c(20L, 80L))
    ),
    source = "recipe",
    component = "step_isomap",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_isomap <- function(x, ...) {
  c("dimRed", "RSpectra", "igraph", "RANN")
}
