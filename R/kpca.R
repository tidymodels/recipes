#' Kernel PCA Signal Extraction
#'
#' `step_kpca` a *specification* of a recipe step that
#'  will convert numeric data into one or more principal components
#'  using a kernel basis expansion.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the components. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? By default, the function assumes
#'  that the new principal component columns created by the original
#'  variables will be used as predictors in a model.
#' @param num_comp The number of PCA components to retain as new
#'  predictors. If `num_comp` is greater than the number of columns
#'  or the number of possible components, a smaller value will be
#'  used.
#' @param options A list of options to
#'  [kernlab::kpca()]. Defaults are set for the arguments
#'  `kernel` and `kpar` but others can be passed in.
#'  **Note** that the arguments `x` and `features`
#'  should not be passed here (or at all).
#' @param res An S4 [kernlab::kpca()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @keywords datagen
#' @concept preprocessing
#' @concept pca
#' @concept projection_methods
#' @concept kernel_methods
#' @concept basis_expansion
#' @export
#' @details Kernel principal component analysis (kPCA) is an
#'  extension a PCA analysis that conducts the calculations in a
#'  broader dimensionality defined by a kernel function. For
#'  example, if a quadratic kernel function were used, each variable
#'  would be represented by its original values as well as its
#'  square. This nonlinear mapping is used during the PCA analysis
#'  and can potentially help find better representations of the
#'  original data.
#'
#' This step requires the \pkg{dimRed} and \pkg{kernlab} packages.
#' If not installed, the step will stop with a note about installing
#' these packages.
#'
#' As with ordinary PCA, it is important to standardized the
#'  variables prior to running PCA (`step_center` and
#'  `step_scale` can be used for this purpose).
#'
#' When performing kPCA, the kernel function (and any important
#'  kernel parameters) must be chosen. The \pkg{kernlab} package is
#'  used and the reference below discusses the types of kernels
#'  available and their parameter(s). These specifications can be
#'  made in the `kernel` and `kpar` slots of the
#'  `options` argument to `step_kpca`.
#'
#' The argument `num_comp` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_comp < 10`, their names will be `kPC1` -
#'  `kPC9`. If `num_comp = 101`, the names would be
#'  `kPC001` - `kPC101`.
#'
#' @references Scholkopf, B., Smola, A., and Muller, K. (1997).
#'  Kernel principal component analysis. *Lecture Notes in
#'  Computer Science*, 1327, 583-588.
#'
#' Karatzoglou, K., Smola, A., Hornik, K., and Zeileis, A. (2004).
#'  kernlab - An S4 package for kernel methods in R. *Journal
#'  of Statistical Software*, 11(1), 1-20.
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
#'   step_YeoJohnson(all_predictors()) %>%
#'   step_normalize(all_predictors()) %>%
#'   step_kpca(all_predictors())
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
#' @seealso [step_pca()] [step_ica()]
#'   [step_isomap()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]
#'
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
           skip = FALSE,
           id = rand_id("kpca")) {

    recipes_pkg_check(c("dimRed", "kernlab"))
    message(
      paste(
        "`step_kpca()` is deprecated in favor of either `step_kpca_rbf()`",
        "or `step_kpca_poly()`. It will be removed in future versions."
      )
    )

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
        skip = skip,
        id = id
      )
    )
}

step_kpca_new <-
  function(terms, role, trained, num_comp, res, options, prefix, skip, id) {
    step(
      subclass = "kpca",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      res = res,
      options = options,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_kpca <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  if (x$num_comp > 0) {
    kprc <- dimRed::kPCA(stdpars = c(list(ndim = x$num_comp), x$options))
    kprc <-
      try(
        kprc@fun(
          dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
          kprc@stdpars
        ),
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
    new_data <- new_data[, !(colnames(new_data) %in% pca_vars), drop = FALSE]
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


#' @rdname step_kpca
#' @param x A `step_kpca` object
#' @export
tidy.step_kpca <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- tibble(terms = colnames(x$res@org.data))
    } else {
      res <- tibble(terms = x$res$x_vars)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

