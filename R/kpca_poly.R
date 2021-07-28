#' Polynomial Kernel PCA Signal Extraction
#'
#' `step_kpca_poly` a *specification* of a recipe step that
#'  will convert numeric data into one or more principal components
#'  using a polynomial kernel basis expansion.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param num_comp The number of PCA components to retain as new
#'  predictors. If `num_comp` is greater than the number of columns
#'  or the number of possible components, a smaller value will be
#'  used.
#' @param degree,scale_factor,offset Numeric values for the polynomial kernel function.
#' @param res An S4 [kernlab::kpca()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @template step-return
#' @keywords datagen
#' @concept preprocessing
#' @concept pca
#' @concept projection_methods
#' @concept kernel_methods
#' @concept basis_expansion
#' @export
#' @details Kernel principal component analysis (kPCA) is an
#'  extension of a PCA analysis that conducts the calculations in a
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
#' As with ordinary PCA, it is important to standardize the
#'  variables prior to running PCA (`step_center` and
#'  `step_scale` can be used for this purpose).
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
#' When you [`tidy()`] this step, a tibble with column `terms` (the
#'  selectors or variables selected) is returned.
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
#'   step_YeoJohnson(all_numeric_predictors()) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_kpca_poly(all_numeric_predictors())
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
step_kpca_poly <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp = 5,
           res = NULL,
           degree = 2,
           scale_factor = 1,
           offset = 1,
           prefix = "kPC",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("kpca_poly")) {

    recipes_pkg_check(required_pkgs.step_kpca_poly())

    add_step(
      recipe,
      step_kpca_poly_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        res = res,
        degree = degree,
        scale_factor = scale_factor,
        offset = offset,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_kpca_poly_new <-
  function(terms, role, trained, num_comp, res, degree, scale_factor, offset,
           prefix, keep_original_cols, skip, id) {
    step(
      subclass = "kpca_poly",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      res = res,
      degree = degree,
      scale_factor = scale_factor,
      offset = offset,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_kpca_poly <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_type(training[, col_names])

  if (x$num_comp > 0) {
    kprc <-
      dimRed::kPCA(
        stdpars = c(
          list(ndim = x$num_comp),
          list(
            kernel = "polydot",
            kpar = list(degree = x$degree, scale = x$scale_factor, offset = x$offset)
          )
        )
      )
    kprc <-
      try(
        kprc@fun(
          dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
          kprc@stdpars
        ),
        silent = TRUE
      )
    if (inherits(kprc, "try-error")) {
      rlang::abort(paste0("`step_kpca_poly` failed with error:\n",
                          as.character(kprc)))
    }
  } else {
    kprc <- list(x_vars = col_names)
  }

  step_kpca_poly_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    degree = x$degree,
    scale_factor = x$scale_factor,
    offset = x$offset,
    res = kprc,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_kpca_poly <- function(object, new_data, ...) {
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

print.step_kpca_poly <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 0) {
      cat("No kPCA components were extracted.\n")
    } else {
      cat("Polynomial kernel PCA (", x$res@pars$kernel, ") extraction with ", sep = "")
      cat(format_ch_vec(colnames(x$res@org.data), width = width))
    }
  } else {
    cat("Polynomial kernel PCA extraction with ", sep = "")
    cat(format_selectors(x$terms, width = width))
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' @rdname tidy.recipe
#' @param x A `step_kpca_poly` object
#' @export
tidy.step_kpca_poly <- function(x, ...) {
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


#' @rdname tunable.step
#' @export
tunable.step_kpca_poly <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "degree", "scale_factor", "offset"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "degree"),
      list(pkg = "dials", fun = "scale_factor"),
      list(pkg = "dials", fun = "offset")
    ),
    source = "recipe",
    component = "step_kpca_poly",
    component_id = x$id
  )
}


#' @rdname required_pkgs.step
#' @export
required_pkgs.step_kpca_poly <- function(x, ...) {
  c("dimRed", "kernlab")
}