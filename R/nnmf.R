#' Non-Negative Matrix Factorization Signal Extraction
#'
#' @description
#'
#' `step_nnmf` creates a *specification* of a recipe step
#'  that will convert numeric data into one or more non-negative
#'  components.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [step_nnmf_sparse()] instead of this step function.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param num_run A positive integer for the number of computations runs used
#'  to obtain a consensus projection.
#' @param options A list of options to `nmf()` in the NMF package by way of the
#'  `NNMF()` function in the `dimRed` package. **Note** that the arguments
#'  `data` and `ndim` should not be passed here, and that NMF's parallel
#'  processing is turned off in favor of resample-level parallelization.
#' @param res The `NNMF()` object is stored
#'  here once this preprocessing step has been trained by
#'  [prep()].
#' @param columns A character string of variable names that will
#'  be populated elsewhere.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @param seed An integer that will be used to set the seed in isolation
#'  when computing the factorization.
#' @template step-return
#' @family multivariate transformation steps
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
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#' `terms` (the selectors or variables selected) and the number of
#' components is returned.
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_nnmf"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2"))
#' data(biomass, package = "modeldata")
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
step_nnmf <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp = 2,
           num_run = 30,
           options = list(),
           res = NULL,
           columns = NULL,
           prefix = "NNMF",
           seed = sample.int(10^5, 1),
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("nnmf")) {
    recipes_pkg_check(required_pkgs.step_nnmf())
    lifecycle::deprecate_warn("0.2.0", "step_nnmf()", "step_nnmf_sparse()")
    add_step(
      recipe,
      step_nnmf_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        num_run = num_run,
        options = options,
        res = res,
        columns = columns,
        prefix = prefix,
        seed = seed,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_nnmf_new <-
  function(terms, role, trained, num_comp, num_run, options, res, columns,
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
      columns = columns,
      prefix = prefix,
      seed = seed,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_nnmf <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  if (x$num_comp > 0 && length(col_names) > 0) {
    x$num_comp <- min(x$num_comp, length(col_names))

    nmf_opts <- list(parallel = FALSE, parallel.required = FALSE)

    nnm <- try(
      eval_dimred_call(
        "embed",
        .method = "NNMF",
        .data = dimred_data(training[, col_names, drop = FALSE]),
        ndim = x$num_comp,
        nrun = x$num_run,
        seed = x$seed,
        .mute = c("message", "output"),
        options = x$options,
        .options = nmf_opts
      ),
      silent = TRUE
    )
    if (inherits(nnm, "try-error")) {
      rlang::abort(paste0("`step_nnmf` failed with error:\n", as.character(nnm)))
    }
  } else {
    nnm <- NULL
  }

  step_nnmf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    num_run = x$num_run,
    options = x$options,
    res = nnm,
    columns = col_names,
    prefix = x$prefix,
    seed = x$seed,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_nnmf <- function(object, new_data, ...) {

  if (object$num_comp > 0 && length(object$columns) > 0) {
    check_new_data(object$columns, object, new_data)
    nnmf_vars <- rownames(object$res@other.data$w)
    comps <-
      object$res@apply(dimred_data(new_data[, nnmf_vars, drop = FALSE]))@data
    comps <- comps[, seq_len(object$num_comp), drop = FALSE]
    colnames(comps) <- names0(ncol(comps), object$prefix)
    comps <- as_tibble(comps)
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, comps)
    keep_original_cols <- get_keep_original_cols(object)

    if (!keep_original_cols) {
      new_data <- new_data[, !(colnames(new_data) %in% nnmf_vars), drop = FALSE]
    }
  }
  new_data
}


print.step_nnmf <- function(x, width = max(20, options()$width - 29), ...) {
  title <- "Non-negative matrix factorization for "
  print_step(colnames(x$res@org.data), x$terms, x$trained, title, width)
  invisible(x)
}


#' @rdname tidy.recipe
#' @export
tidy.step_nnmf <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0 && length(x$columns) > 0) {
      res <- x$res@other.data$w
      var_nms <- rownames(res)
      res <- tibble::as_tibble(res)
      res$terms <- var_nms
      res <- tidyr::pivot_longer(res,
        cols = c(-terms),
        names_to = "component", values_to = "value"
      )
      res <- res[, c("terms", "value", "component")]
      res <- res[order(res$component, res$terms), ]
    } else {
      res <- tibble(terms = unname(x$columns), value = na_dbl, component = na_dbl)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = x$num_comp)
  }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_nnmf <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "num_run"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "num_runs", range = c(1L, 10L))
    ),
    source = "recipe",
    component = "step_nnmf",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_nnmf <- function(x, ...) {
  c("dimRed", "NMF")
}
