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
#' @param options A list of options to [NMF::nmf()] by way of
#'  [dimRed::NNMF()]. **Note** that the arguments
#'  `data` and `ndim` should not be passed here.
#' @param res The [dimRed::NNMF()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @param seed An integer that will be used to set the seed in isolation
#'  when computing the factorization.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and the number of components.
#' @keywords datagen
#' @concept preprocessing nnmf projection_methods
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
#' \donttest{
#' data(biomass)
#'
#' if (require(dimRed) & require(NMF)) {
#'   rec <- recipe(HHV ~ ., data = biomass) %>%
#'     update_role(sample, new_role = "id var") %>%
#'     update_role(dataset, new_role = "split variable") %>%
#'     step_nnmf(all_predictors(), num_comp = 2, seed = 473, num_run = 2) %>%
#'     prep(training = biomass, retain = TRUE)
#'
#'   juice(rec)
#'
#'   library(ggplot2)
#'   ggplot(juice(rec), aes(x = NNMF2, y = NNMF1, col = HHV)) + geom_point()
#' }
#' }
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
           skip = FALSE,
           id = rand_id("nnmf")
           ) {
    recipes_pkg_check(c("dimRed", "fastICA"))
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
        skip = skip,
        id = id
      )
    )
  }

step_nnmf_new <-
  function(terms, role, trained, num_comp, num_run,
           options, res, prefix, seed, skip, id) {
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
      skip = skip,
      id = id
    )
  }

#' @importFrom withr with_seed
#' @export
prep.step_nnmf <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  x$num_comp <- min(x$num_comp, length(col_names))

  opts <- list(options = x$options)
  opts$ndim <- x$num_comp
  opts$nrun <- x$num_run
  opts$seed <- x$seed
  opts$.mute <- c("message", "output")
  opts$.data <- dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE]))
  opts$.method <- "NNMF"

  suppressPackageStartupMessages(loadNamespace("NMF"))

 nnm <- do.call(dimRed::embed, opts)

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
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_nnmf <- function(object, new_data, ...) {
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
  new_data <-
    new_data[, !(colnames(new_data) %in% nnmf_vars), drop = FALSE]
  as_tibble(new_data)
}


print.step_nnmf <-
  function(x, width = max(20, options()$width - 29), ...) {
    cat("Non-negative matrix factorization for ")
    printer(colnames(x$res@org.data), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_nnmf
#' @param x A `step_nnmf` object.
tidy.step_nnmf <- function(x, ...) {
  if (is_trained(x)) {
    var_names <- colnames(x$res@other.data$H)
    res <- tibble(terms = var_names, components  = x$num_comp)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, components  = x$num_comp)
  }
  res$id <- x$id
  res
}

