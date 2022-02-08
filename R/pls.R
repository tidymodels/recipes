#' Partial Least Squares Feature Extraction
#'
#' `step_pls` creates a *specification* of a recipe step that will
#'  convert numeric data into one or more new dimensions.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param predictor_prop The maximum number of original predictors that can have
#'  non-zero coefficients for each PLS component (via regularization).
#' @param preserve Use `keep_original_cols` instead to specify whether the
#'  original predictor data should be retained along with the new features.
#' @param outcome When a single outcome is available, character
#'  string or call to [dplyr::vars()] can be used to specify a single outcome
#'  variable.
#' @param options A list of options to `mixOmics::pls()`, `mixOmics::spls()`,
#' `mixOmics::plsda()`, or `mixOmics::splsda()` (depending on the data and
#' arguments).
#' @param res A list of results are stored here once this preprocessing step
#'  has been trained by [prep()].
#' @param columns A character string of variable names that will
#'  be populated elsewhere.
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details PLS is a supervised version of principal component
#'  analysis that requires the outcome data to compute
#'  the new features.
#'
#' This step requires the Bioconductor \pkg{mixOmics} package. If not installed, the
#'  step will stop with a note about installing the package.
#'
#' The argument `num_comp` controls the number of components that will
#'  be retained (the original variables that are used to derive the
#'  components are removed from the data). The new components will
#'  have names that begin with `prefix` and a sequence of numbers.
#'  The variable names are padded with zeros. For example, if `num_comp <
#'  10`, their names will be `PLS1` - `PLS9`. If `num_comp = 101`, the
#'  names would be `PLS001` - `PLS101`.
#'
#' Sparsity can be encouraged using the `predictor_prop` parameter. This affects
#' each PLS component, and indicates the maximum proportion of predictors with
#' non-zero coefficients in each component. `step_pls()` converts this
#' proportion to determine the `keepX` parameter in `mixOmics::spls()` and
#' `mixOmics::splsda()`. See the references in `mixOmics::spls()` for details.
#'
#' # Tidying
#'
#' The [`tidy()`][tidy.recipe()] method returns the coefficients that are
#' usually defined as
#'
#' \deqn{W(P'W)^{-1}}
#'
#' (See the Wikipedia article below)
#'
#' When applied to data, these values are usually scaled by a column-specific
#' norm. The `tidy()` method applies this same norm to the coefficients shown
#' above. When you `tidy()` this step, a tibble with columns `terms` (the
#' selectors or variables selected), `components`, and `values` is returned.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Partial_least_squares_regression}
#'
#' Rohart F, Gautier B, Singh A, Lê Cao K-A (2017) _mixOmics: An R package for
#' 'omics feature selection and multiple data integration_. PLoS Comput Biol
#' 13(11): e1005752. \doi{10.1371/journal.pcbi.1005752}
#' @examples
#' # requires the Bioconductor mixOmics package
#' data(biomass, package = "modeldata")
#'
#' biom_tr <-
#'   biomass %>%
#'   dplyr::filter(dataset == "Training") %>%
#'   dplyr::select(-dataset,-sample)
#' biom_te <-
#'   biomass %>%
#'   dplyr::filter(dataset == "Testing")  %>%
#'   dplyr::select(-dataset,-sample,-HHV)
#'
#' dense_pls <-
#'   recipe(HHV ~ ., data = biom_tr) %>%
#'   step_pls(all_numeric_predictors(), outcome = "HHV", num_comp = 3)
#'
#' sparse_pls <-
#'   recipe(HHV ~ ., data = biom_tr) %>%
#'   step_pls(all_numeric_predictors(), outcome = "HHV", num_comp = 3, predictor_prop = 4/5)
#'
#' ## -----------------------------------------------------------------------------
#' ## PLS discriminant analysis
#'
#' data(cells, package = "modeldata")
#'
#' cell_tr <-
#'   cells %>%
#'   dplyr::filter(case == "Train") %>%
#'   dplyr::select(-case)
#' cell_te <-
#'   cells %>%
#'   dplyr::filter(case == "Test")  %>%
#'   dplyr::select(-case,-class)
#'
#' dense_plsda <-
#'   recipe(class ~ ., data = cell_tr) %>%
#'   step_pls(all_numeric_predictors(), outcome = "class", num_comp = 5)
#'
#' sparse_plsda <-
#'   recipe(class ~ ., data = cell_tr) %>%
#'   step_pls(all_numeric_predictors(), outcome = "class", num_comp = 5, predictor_prop = 1/4)
#'
step_pls <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 2,
           predictor_prop = 1,
           outcome = NULL,
           options = list(scale = TRUE),
           preserve = deprecated(),
           res = NULL,
           columns = NULL,
           prefix = "PLS",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("pls")) {
    if (is.null(outcome)) {
      rlang::abort("`outcome` should select at least one column.")
    }

    if (lifecycle::is_present(preserve)) {
      lifecycle::deprecate_warn(
        "0.1.16",
        "step_pls(preserve = )",
        "step_pls(keep_original_cols = )"
      )
      keep_original_cols <- preserve
    }

    recipes_pkg_check(required_pkgs.step_pls())

    add_step(
      recipe,
      step_pls_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        predictor_prop = predictor_prop,
        outcome = outcome,
        options = options,
        preserve = keep_original_cols,
        res = res,
        columns = columns,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_pls_new <-
  function(terms, role, trained, num_comp, predictor_prop, outcome, options,
           preserve, res, columns, prefix, keep_original_cols, skip, id) {
    step(
      subclass = "pls",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      predictor_prop = predictor_prop,
      outcome = outcome,
      options = options,
      preserve = preserve,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }


## -----------------------------------------------------------------------------
## Taken from plsmod

pls_fit <- function(x, y, ncomp = NULL, keepX = NULL, ...) {
  dots <- rlang::enquos(...)
  p <- ncol(x)
  if (is.null(keepX)) {
    keepX <- p
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(ncomp)) {
    ncomp <- p
  } else {
    ncomp <- min(ncomp, p)
  }
  if (all(keepX < p) && length(keepX) == 1) {
    keepX <- rep(min(keepX, p), ncomp)
  }
  if (is.factor(y)) {
    if (all(keepX == p)) {
      cl  <- rlang::call2("plsda", .ns = "mixOmics", X = quote(x), Y = quote(y), ncomp = ncomp, !!!dots)
    } else {
      cl  <- rlang::call2("splsda", .ns = "mixOmics", X = quote(x), Y = quote(y), ncomp = ncomp, keepX = keepX, !!!dots)
    }
  } else {
    if (all(keepX == p)) {
      cl  <- rlang::call2("pls", .ns = "mixOmics", X = quote(x), Y = quote(y), ncomp = ncomp, !!!dots)
    } else {
      cl  <- rlang::call2("spls", .ns = "mixOmics", X = quote(x), Y = quote(y), ncomp = ncomp, keepX = keepX, !!!dots)
    }
  }
  res <- rlang::eval_tidy(cl)
  res
}

make_pls_call <- function(ncomp, keepX, args = NULL) {
  cl <-
    rlang::call2(
      "pls_fit",
      x = rlang::expr(as.matrix(training[, x_names])),
      y = rlang::expr(training[[y_names]]),
      ncomp = ncomp,
      keepX = keepX,
      !!!args
    )
  cl
}

get_norms <- function(x) norm(x, type = "2")^2

butcher_pls <- function(x) {
  if (inherits(x, "try-error")) {
    return(NULL)
  }
  .mu <- attr(x$X, "scaled:center")
  .sd <- attr(x$X, "scaled:scale")
  W <- x$loadings$X  # W matrix, P = X'rot
  variates <- x$variates[["X"]]
  P <- crossprod(x$X, variates)
  coefs <- W %*% solve(t(P) %*%  W)

  col_norms <- apply(variates, 2, get_norms)

  list(mu = .mu, sd = .sd, coefs = coefs, col_norms = col_norms)
}

pls_project <- function(object, x) {
  pls_vars <- names(object$mu)
  x <- x[, pls_vars]
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  z <- sweep(x, 2, STATS = object$mu, "-")
  z <- sweep(z, 2, STATS = object$sd, "/")
  res <- z %*% object$coefs
  res <- tibble::as_tibble(res)
  res <- purrr::map2_dfc(res, object$col_norms, ~ .x * .y)
  res
}

old_pls_project <- function(object, x) {
  pls_vars <- rownames(object$projection)
  n <- nrow(x)
  input_data <- as.matrix(x[, pls_vars])
  if (!all(is.na(object$scale))) {
    input_data <- sweep(input_data, 2, object$scale,  "/")
  }
  input_data <- sweep(input_data, 2, object$Xmeans,  "-")
  comps <- input_data %*% object$projection
  colnames(comps) <- paste0("pls", 1:ncol(comps))
  tibble::as_tibble(comps)
}

pls_worked <- function(x) {
  !isTRUE(all.equal(names(x), c("x_vars", "y_vars")))
}

use_old_pls <- function(x) {
  any(names(x) == "Xmeans")
}


prop2int <- function(x, p) {
  cuts <- seq(0, p, length.out = p + 1)
  as.integer(cut(x * p, breaks = cuts, include.lowest = TRUE))
}

get_columns_pls <- function(x) {
  if (use_old_pls(x$res)) {
    rownames(x$res$projection)
  } else {
    x$columns
  }
}

## -----------------------------------------------------------------------------

#' @export
prep.step_pls <- function(x, training, info = NULL, ...) {
  x_names <- recipes_eval_select(x$terms, training, info)
  y_names <- recipes_eval_select(x$outcome, training, info)

  check_type(training[, x_names])
  if (length(y_names) > 1 ) {
    rlang::abort("`step_pls()` only supports univariate models.")
  }

  if (x$num_comp > 0 && length(x_names) > 0) {
    ncomp <- min(x$num_comp,  length(x_names))
    # Convert proportion to number of terms
    x$predictor_prop <- max(x$predictor_prop, 0.00001)
    x$predictor_prop <- min(x$predictor_prop, 1)
    nterm <- prop2int(x$predictor_prop, length(x_names))

    cl <- make_pls_call(ncomp, nterm, x$options)
    res <- try(rlang::eval_tidy(cl), silent = TRUE)
    if (inherits(res, "try-error")) {
      rlang::warn(paste0("`step_pls()` failed: ", as.character(res)))
      res <- list(x_vars = x_names, y_vars = y_names)
    } else {
      res <- butcher_pls(res)
    }

  } else {
    res <- NULL
  }

  step_pls_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    predictor_prop = x$predictor_prop,
    outcome = x$outcome,
    options = x$options,
    preserve = x$preserve,
    res = res,
    columns = x_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pls <- function(object, new_data, ...) {
  if (object$num_comp > 0 && length(get_columns_pls(object)) > 0 && pls_worked(object$res)) {

    if (use_old_pls(object$res)) {
      comps <- old_pls_project(object$res, new_data)
    } else {
      comps <- pls_project(object$res, new_data)
    }

    names(comps) <- names0(ncol(comps), object$prefix)
    comps <- check_name(comps, new_data, object)

    new_data <- bind_cols(new_data, as_tibble(comps))
    keep_original_cols <- get_keep_original_cols(object)

    # Old pls never preserved original columns,
    # but didn't have the `preserve` option
    if (use_old_pls(object$res)) {
      pls_vars <- rownames(object$res$projection)
      keep_vars <- !(colnames(new_data) %in% pls_vars)
      new_data <- new_data[, keep_vars, drop = FALSE]
    } else if (any(!object$preserve, !keep_original_cols)) {
      pls_vars <- names(object$res$mu)
      keep_vars <- !(colnames(new_data) %in% pls_vars)
      new_data <- new_data[, keep_vars, drop = FALSE]
    }

    if (!is_tibble(new_data)) {
      new_data <- as_tibble(new_data)
    }
  }
  new_data
}


print.step_pls <- function(x, width = max(20, options()$width - 35), ...) {
  title <- "PLS feature extraction with "
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}


#' @rdname tidy.recipe
#' @export
tidy.step_pls <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0 && length(get_columns_pls(x)) > 0) {
      res <-
        purrr::map2_dfc(as.data.frame(x$res$coefs), x$res$col_norms, ~ .x * .y) %>%
        dplyr::mutate(terms = rownames(x$res$coefs)) %>%
        tidyr::pivot_longer(c(-terms), names_to = "component", values_to = "value")
      res <- res[, c("terms", "value", "component")]
      res$component <- gsub("comp", "PLS", res$component)
    } else {
      res <- tibble(terms = unname(get_columns_pls(x)), value = na_dbl, component  = na_chr)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = na_chr)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_pls <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "predictor_prop"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "predictor_prop")
    ),
    source = "recipe",
    component = "step_pls",
    component_id = x$id
  )
}


#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_pls <- function(x, ...) {
  c("mixOmics")
}
