#' Partial Least Squares Feature Extraction
#'
#' `step_pls` creates a *specification* of a recipe step that will
#'  convert numeric data into one or more new dimensions.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the dimensions. See
#'  [selections()] for more details. For the `tidy` method, these
#'  are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new dimension columns created by the original variables
#'  will be used as predictors in a model.
#' @param num_comp The number of pls dimensions to retain as new
#'  predictors. If `num_comp` is greater than the number of columns
#'  or the number of possible dimensions, a smaller value will be
#'  used.
#' @param outcome When a single outcome is available, character
#'  string or call to [dplyr::vars()] can be used to specify the
#'  variable. When there are multipole outcomes, [dplyr::vars()]
#'  must be used. This that can include specific variable names
#'  separated by commas or different selectors (see [selections()]).
#' @param options A list of options to [pls::plsr()].
#' @param res The [pls::plsr()] object is stored
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
#' @concept pls
#' @concept projection_methods
#' @export
#' @details PLS is a supervised version of principal component
#'  analysis that requires one or more numeric outcomes to compute
#'  the new features. The data should be scaled (and perhaps
#'  centered) prior to running these calculations.
#'
#' This step requires the \pkg{pls} package. If not installed, the
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
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' pls_rec <- recipe(HHV ~ ., data = biomass_tr) %>%
#'   step_rm(sample, dataset) %>%
#'   step_normalize(all_predictors()) %>%
#'   # If the outcome(s) need standardization, do it in separate
#'   # steps with skip = FALSE so that new data where the
#'   # outcome is missing can be processed.
#'   step_normalize(all_outcomes(), skip = TRUE) %>%
#'   step_pls(all_predictors(), outcome = "HHV")
#'
#' pls_rec <- prep(pls_rec, training = biomass_tr)
#'
#' pls_test_scores <- bake(pls_rec, new_data = biomass_te[, -8])
#'
#' tidy(pls_rec, number = 4)
#' @seealso [step_pca()] [step_kpca()]
#'   [step_ica()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_pls <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 2,
           num_terms = 2,
           outcome = NULL,
           options = list(scale = TRUE),
           preserve = FALSE,
           res = NULL,
           prefix = "PLS",
           skip = FALSE,
           id = rand_id("pls")) {
    if (is.null(outcome)) {
      rlang::abort("`outcome` should select at least one column.")
    }

    recipes_pkg_check("mixOmics")

    add_step(
      recipe,
      step_pls_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        num_terms = num_terms,
        outcome = outcome,
        options = options,
        preserve = preserve,
        res = res,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_pls_new <-
  function(terms, role, trained, num_comp, num_terms, outcome, options,
           preserve, res, prefix, skip, id) {
    step(
      subclass = "pls",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      num_terms = num_terms,
      outcome = outcome,
      options = options,
      preserve = preserve,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

## -----------------------------------------------------------------------------
## Taken from plsmod

pls_fit <- function(x, y, ncomp = NULL, keepX = NULL, ...) {
  p <- ncol(x)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(ncomp)) {
    ncomp <- p
  } else {
    ncomp <- min(ncomp, p)
  }
  if (!is.null(keepX) && length(keepX) == 1) {
    keepX <- rep(min(keepX, p), p)
  }
  if (is.factor(y)) {
    if (is.null(keepX)) {
      res <- mixOmics::plsda(X = x, Y = y, ncomp = ncomp, ...)
    } else {
      res <- mixOmics::splsda(X = x, Y = y, ncomp = ncomp, keepX = keepX, ...)
    }
  } else {
    if (is.null(keepX)) {
      res <- mixOmics::pls(X = x, Y = y, ncomp = ncomp, ...)
    } else {
      res <- mixOmics::spls(X = x, Y = y, ncomp = ncomp, keepX = keepX, ...)
    }
  }
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

#' @export
prep.step_pls <- function(x, training, info = NULL, ...) {
  x_names <- terms_select(x$terms,   info = info)
  y_names <- terms_select(x$outcome, info = info)
  check_type(training[, x_names])
  if (length(y_names) > 1 ) {
    rlang::abort("`step_pls()` only supports univariate models.")
  }

  if (x$num_comp > 0) {
    ncomp <- min(x$num_comp,  length(x_names))
    nterm <- min(x$num_terms, length(x_names))
    cl <- make_pls_call(ncomp, nterm, x$options)
    res <- try(rlang::eval_tidy(cl), silent = TRUE)
    if (inherits(res, "try-error")) {
      rlang::warn(paste0("`step_pls()` failed: ", as.character(res)))
      res <- list(x_vars = x_names, y_vars = y_names)
    } else {
      res <- butcher_pls(res)
    }

  } else {
    res <- list(x_vars = x_names, y_vars = y_names)
  }

  step_pls_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    num_terms = x$num_terms,
    outcome = x$outcome,
    options = x$options,
    preserve = x$preserve,
    res = res,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pls <- function(object, new_data, ...) {
  if (object$num_comp > 0 & any(names(object$res) == "coefs")) {
    pls_vars <- names(object$res$mu)
    comps <- pls_project(object$res, new_data[, pls_vars])
    names(comps) <- names0(ncol(comps), object$prefix)
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, as_tibble(comps))
    if (!object$preserve) {
      new_data <- new_data[, !(colnames(new_data) %in% pls_vars), drop = FALSE]
    }
    if (!is_tibble(new_data))
      new_data <- as_tibble(new_data)
  }
  new_data
}


print.step_pls <- function(x, width = max(20, options()$width - 35), ...) {
  if (x$num_comp == 0) {
    cat("No PLS components were extracted.\n")
  } else {
    cat("PLS feature extraction with ")
    # printer(rownames(x$res$projection), x$terms, x$trained, width = width)
  }
  invisible(x)
}


#' @rdname step_pls
#' @param x A `step_pls` object
#' @export
tidy.step_pls <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- as.data.frame(x$res$projection)
      res <- stack(res)
      res$terms <- rep(rownames(x$res$projection), ncol(x$res$projection))
      names(res)[1:2] <- c("value", "component")
      res <- res[, c("terms", "value", "component")]
      res$component <- gsub("Comp ", "PLS", res$component)
    } else {
      res <- tibble(terms = x$res$x_vars, value = na_dbl, component  = na_chr)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = na_chr)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.step
#' @export
tunable.step_pls <- function(x, ...) {
  tibble::tibble(
    name = "num_comp",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1L, 4L))),
    source = "recipe",
    component = "step_pls",
    component_id = x$id
  )
}
