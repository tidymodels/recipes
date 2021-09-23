#' Kernel PCA Signal Extraction
#'
#' `step_kpca` creates a *specification* of a recipe step that
#'  will convert numeric data into one or more principal components
#'  using a kernel basis expansion.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param options A list of options to [kernlab::kpca()]. Defaults are set for
#'  the arguments `kernel` and `kpar` but others can be passed in.
#'  **Note** that the arguments `x` and `features` should not be passed here
#'  (or at all).
#' @param res An S4 [kernlab::kpca()] object is stored here once this
#'  preprocessing step has be trained by [`prep()`][prep.recipe()].
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#' When performing kPCA with `step_kpca()`, you must choose the kernel
#' function (and any important kernel parameters). This step uses the
#' \pkg{kernlab} package; the reference below discusses the types of kernels
#' available and their parameter(s). These specifications can be made in the
#' `kernel` and `kpar` slots of the `options` argument to `step_kpca()`.
#' Consider using [step_kpca_rbf()] for a radial basis function kernel or
#' [step_kpca_poly()] for a polynomial kernel.
#'
#' @template kpca-info
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
#' kpca_trans <- rec %>%
#'   step_YeoJohnson(all_numeric_predictors()) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_kpca(all_numeric_predictors())
#'
#' if (require(dimRed) & require(kernlab)) {
#'   kpca_estimates <- prep(kpca_trans, training = biomass_tr)
#'
#'   kpca_te <- bake(kpca_estimates, biomass_te)
#'
#'   rng <- extendrange(c(kpca_te$kPC1, kpca_te$kPC2))
#'   plot(kpca_te$kPC1, kpca_te$kPC2,
#'        xlim = rng, ylim = rng)
#'
#'   tidy(kpca_trans, number = 3)
#'   tidy(kpca_estimates, number = 3)
#' }
step_kpca <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 5,
           res = NULL,
           options = list(kernel = "rbfdot",
                          kpar = list(sigma = 0.2)),
           prefix = "kPC",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("kpca")) {

    recipes_pkg_check(required_pkgs.step_kpca())

    add_step(
      recipe,
      step_kpca_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        res = res,
        options = options,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
}

step_kpca_new <-
  function(terms, role, trained, num_comp, res, options, prefix,
           keep_original_cols, skip, id) {
    step(
      subclass = "kpca",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      res = res,
      options = options,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_kpca <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])

  if (x$num_comp > 0) {
    kprc <- dimRed::kPCA(stdpars = c(list(ndim = x$num_comp), x$options))
    kprc <-
      try(
        suppressMessages({
          kprc@fun(
            dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
            kprc@stdpars
          )
        }),
        silent = TRUE
      )
    if (inherits(kprc, "try-error")) {
      rlang::abort(paste0("`step_kpca` failed with error:\n", as.character(kprc)))
    }
  } else {
    kprc <- list(x_vars = col_names)
  }

  step_kpca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    options = x$options,
    res = kprc,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_kpca <- function(object, new_data, ...) {
  if (object$num_comp > 0) {
    pca_vars <- colnames(environment(object$res@apply)$indata)
    comps <- object$res@apply(
      dimRed::dimRedData(as.data.frame(new_data[, pca_vars, drop = FALSE]))
    )@data
    comps <- comps[, 1:object$num_comp, drop = FALSE]
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, as_tibble(comps))
    keep_original_cols <- get_keep_original_cols(object)

    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% pca_vars), drop = FALSE]
    }
  }
  as_tibble(new_data)
}

print.step_kpca <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 0) {
      cat("No kPCA components were extracted.\n")
    } else {
      cat("Kernel PCA (", x$res@pars$kernel, ") extraction with ", sep = "")
      cat(format_ch_vec(colnames(x$res@org.data), width = width))
    }
  } else {
    cat("Kernel PCA extraction with ", sep = "")
    cat(format_selectors(x$terms, width = width))
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' @rdname tidy.recipe
#' @export
tidy.step_kpca <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- tibble(terms = colnames(x$res@org.data))
    } else {
      res <- tibble(terms = unname(x$res$x_vars))
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}



#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_kpca <- function(x, ...) {
  c("dimRed", "kernlab")
}
