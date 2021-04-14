#' NNMF Signal Extraction
#'
#' `step_nnmf` creates a *specification* of a recipe step
#'  that will convert numeric data into one or more non-negative
#'  components.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the components. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new component columns created by the
#'  original variables will be used as predictors in a model.
#' @param num_comp The number of components to retain as new
#'  predictors. If `num_comp` is greater than the number of columns
#'  or the number of possible components, a smaller value will be
#'  used.
#' @param num_run A positive integer for the number of computations runs used
#'  to obtain a consensus projection.
#' @param options A list of options to `nmf()` in the NMF package by way of the
#'  `NNMF()` function in the `dimRed` package. **Note** that the arguments
#'  `data` and `ndim` should not be passed here, and that NMF's parallel
#'  processing is turned off in favor of resample-level parallelization.
#' @param res The `NNMF()` object is stored
#'  here once this preprocessing step has been trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @param seed An integer that will be used to set the seed in isolation
#'  when computing the factorization.
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and the number of components.
#' @keywords datagen
#' @concept preprocessing
#' @concept nnmf
#' @concept projection_methods
#' @export
#' @details Non-negative matrix factorization computes latent components that
#'  have non-negative values and take into account that the original data
#'  have non-negative values.
#'
#' The argument `num_comp` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num < 10`, their names will be `NNMF1` - `NNMF9`.
#'  If `num = 101`, the names would be `NNMF001` -
#'  `NNMF101`.
#'
#' @examples
#'
#' library(modeldata)
#' data(biomass)
#'
#' # rec <- recipe(HHV ~ ., data = biomass) %>%
#' #   update_role(sample, new_role = "id var") %>%
#' #   update_role(dataset, new_role = "split variable") %>%
#' #   step_nnmf(all_numeric_predictors(), num_comp = 2, seed = 473, num_run = 2) %>%
#' #   prep(training = biomass)
#' #
#' # bake(rec, new_data = NULL)
#' #
#' # library(ggplot2)
#' # bake(rec, new_data = NULL) %>%
#' #  ggplot(aes(x = NNMF2, y = NNMF1, col = HHV)) + geom_point()
#'
#' @seealso [step_pca()], [step_ica()], [step_kpca()],
#'   [step_isomap()], [recipe()], [prep.recipe()],
#'   [bake.recipe()]
step_nnmf <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 2,
           num_run = 30,
           options = list(),
           res = NULL,
           prefix = "NNMF",
           seed = sample.int(10^5, 1),
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("nnmf")
           ) {
    recipes_pkg_check(required_pkgs.step_nnmf())
    add_step(
      recipe,
      step_nnmf_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        num_run = num_run,
        options = options,
        res = res,
        prefix = prefix,
        seed = seed,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_nnmf_new <-
  function(terms, role, trained, num_comp, num_run, options, res,
           prefix, seed, keep_original_cols, skip, id) {
    step(
      subclass = "nnmf",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      num_run = num_run,
      options = options,
      res = res,
      prefix = prefix,
      seed = seed,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_nnmf <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  check_type(training[, col_names])

  if (x$num_comp > 0) {

    x$num_comp <- min(x$num_comp, length(col_names))

    opts <- list(options = x$options)
    opts$ndim <- x$num_comp
    opts$nrun <- x$num_run
    opts$seed <- x$seed
    opts$.mute <- c("message", "output")
    opts$.data <- dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE]))
    opts$.method <- "NNMF"
    nmf_opts <- list(parallel = FALSE, parallel.required = FALSE)
    opts$options <- list(.options = nmf_opts)

    nnm <- try(do.call(dimRed::embed, opts), silent = TRUE)
    if (inherits(nnm, "try-error")) {
      rlang::abort(paste0("`step_nnmf` failed with error:\n", as.character(nnm)))
    }
  } else {
    nnm <- list(x_vars = col_names)
  }

  step_nnmf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    num_run = x$num_run,
    options = x$options,
    res = nnm,
    prefix = x$prefix,
    seed = x$seed,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_nnmf <- function(object, new_data, ...) {
  if (object$num_comp > 0) {
    nnmf_vars <- rownames(object$res@other.data$w)
    comps <-
      object$res@apply(
        dimRed::dimRedData(
          as.data.frame(new_data[, nnmf_vars, drop = FALSE])
        )
      )@data
    comps <- comps[, 1:object$num_comp, drop = FALSE]
    colnames(comps) <- names0(ncol(comps), object$prefix)
    new_data <- bind_cols(new_data, as_tibble(comps))
    keep_original_cols <- get_keep_original_cols(object)

    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% nnmf_vars), drop = FALSE]
    }
  }
  as_tibble(new_data)
}


print.step_nnmf <- function(x, width = max(20, options()$width - 29), ...) {
  if (x$num_comp == 0) {
    cat("Non-negative matrix factorization was not done.\n")
  } else {
    cat("Non-negative matrix factorization for ")
    printer(colnames(x$res@org.data), x$terms, x$trained, width = width)
  }
  invisible(x)
}


#' @rdname step_nnmf
#' @param x A `step_nnmf` object.
tidy.step_nnmf <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- x$res@other.data$w
      var_nms <- rownames(res)
      res <- tibble::as_tibble(res)
      res$terms <- var_nms
      res <- tidyr::pivot_longer(res, cols = c(-terms),
                                 names_to = "component", values_to = "value")
      res <- res[,c("terms", "value", "component")]
      res <- res[order(res$component, res$terms),]
    } else {
      res <- tibble(terms = x$res$x_vars, value = na_dbl, component  = na_chr)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component  = x$num_comp)
  }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------

#' @rdname tunable.step
#' @export
tunable.step_nnmf <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "num_run"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "num_run", range = c(1L, 10L))
    ),
    source = "recipe",
    component = "step_nnmf",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_nnmf <- function(x, ...) {
  c("dimRed", "NMF")
}

