#' PCA Signal Extraction
#'
#' `step_pca` creates a *specification* of a recipe step
#'  that will convert numeric data into one or more principal
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
#'  that the new principal component columns created by the original
#'  variables will be used as predictors in a model.
#' @param num The number of PCA components to retain as new
#'  predictors. If `num` is greater than the number of columns
#'  or the number of possible components, a smaller value will be
#'  used.
#' @param threshold A fraction of the total variance that should
#'  be covered by the components. For example, `threshold =
#'  .75` means that `step_pca` should generate enough
#'  components to capture 75\% of the variability in the variables.
#'  Note: using this argument will override and resent any value
#'  given to `num`.
#' @param options A list of options to the default method for
#'  [stats::prcomp()]. Argument defaults are set to
#'  `retx = FALSE`, `center = FALSE`, `scale. =
#'  FALSE`, and `tol = NULL`. **Note** that the argument
#'  `x` should not be passed here (or at all).
#' @param res The [stats::prcomp.default()] object is
#'  stored here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the
#'  loading), and `component`.
#' @keywords datagen
#' @concept preprocessing pca projection_methods
#' @export
#' @details
#' Principal component analysis (PCA) is a transformation of a
#'  group of variables that produces a new set of artificial
#'  features or components. These components are designed to capture
#'  the maximum amount of information (i.e. variance) in the
#'  original variables. Also, the components are statistically
#'  independent from one another. This means that they can be used
#'  to combat large inter-variables correlations in a data set.
#'
#' It is advisable to standardized the variables prior to running
#'  PCA. Here, each variable will be centered and scaled prior to
#'  the PCA calculation. This can be changed using the
#'  `options` argument or by using [step_center()]
#'  and [step_scale()].
#'
#' The argument `num` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num < 10`, their names will be `PC1` - `PC9`.
#'  If `num = 101`, the names would be `PC001` -
#'  `PC101`.
#'
#' Alternatively, `threshold` can be used to determine the
#'  number of components that are required to capture a specified
#'  fraction of the total variance in the variables.
#'
#' @references Jolliffe, I. T. (2010). *Principal Component
#'  Analysis*. Springer.
#'
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' pca_trans <- rec %>%
#'   step_center(all_numeric()) %>%
#'   step_scale(all_numeric()) %>%
#'   step_pca(all_numeric(), num = 3)
#' pca_estimates <- prep(pca_trans, training = USArrests)
#' pca_data <- bake(pca_estimates, USArrests)
#'
#' rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
#' plot(pca_data$PC1, pca_data$PC2,
#'      xlim = rng, ylim = rng)
#'
#' with_thresh <- rec %>%
#'   step_center(all_numeric()) %>%
#'   step_scale(all_numeric()) %>%
#'   step_pca(all_numeric(), threshold = .99)
#' with_thresh <- prep(with_thresh, training = USArrests)
#' bake(with_thresh, USArrests)
#'
#' tidy(pca_trans, number = 3)
#' tidy(pca_estimates, number = 3)
#' @seealso [step_ica()] [step_kpca()]
#'   [step_isomap()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_pca <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     num  = 5,
                     threshold = NA,
                     options = list(),
                     res = NULL,
                     prefix = "PC",
                     skip = FALSE) {
  if (!is.na(threshold) && (threshold > 1 | threshold <= 0))
    stop("`threshold` should be on (0, 1].", call. = FALSE)
  add_step(
    recipe,
    step_pca_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      num = num,
      threshold = threshold,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip
    )
  )
}

step_pca_new <- function(terms = NULL,
                         role = "predictor",
                         trained = FALSE,
                         num  = NULL,
                         threshold = NULL,
                         options = NULL,
                         res = NULL,
                         prefix = "PC",
                         skip = FALSE) {
  step(
    subclass = "pca",
    terms = terms,
    role = role,
    trained = trained,
    num = num,
    threshold = threshold,
    options = options,
    res = res,
    prefix = prefix,
    skip = skip
  )
}

#' @importFrom stats prcomp
#' @importFrom rlang expr
#' @export
prep.step_pca <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  prc_call <-
    expr(prcomp(
      retx = FALSE,
      center = FALSE,
      scale. = FALSE,
      tol = NULL
    ))
  if (length(x$options) > 0)
    prc_call <- mod_call_args(prc_call, args = x$options)
  prc_call$x <- expr(training[, col_names, drop = FALSE])
  prc_obj <- eval(prc_call)

  x$num <- min(x$num, length(col_names))
  if (!is.na(x$threshold)) {
    total_var <- sum(prc_obj$sdev ^ 2)
    num_comp <-
      which.max(cumsum(prc_obj$sdev ^ 2 / total_var) >= x$threshold)
    if (length(num_comp) == 0)
      num_comp <- length(prc_obj$sdev)
    x$num <- num_comp
  }
  ## decide on removing prc elements that aren't used in new projections
  ## e.g. `sdev` etc.

  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    threshold = x$threshold,
    options = x$options,
    res = prc_obj,
    prefix = x$prefix,
    skip = x$skip
  )
}

#' @importFrom tibble as_tibble
#' @export
bake.step_pca <- function(object, newdata, ...) {
  pca_vars <- rownames(object$res$rotation)
  comps <- predict(object$res, newdata = newdata[, pca_vars])
  comps <- comps[, 1:object$num, drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- bind_cols(newdata, as_tibble(comps))
  newdata <-
    newdata[, !(colnames(newdata) %in% pca_vars), drop = FALSE]
  as_tibble(newdata)
}

print.step_pca <-
  function(x, width = max(20, options()$width - 29), ...) {
    cat("PCA extraction with ")
    printer(rownames(x$res$rotation), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom utils stack
#' @rdname step_pca
#' @param x A `step_pca` object.
tidy.step_pca <- function(x, ...) {
  if (is_trained(x)) {
    rot <- as.data.frame(x$res$rotation)
    vars <- rownames(rot)
    npc <- ncol(rot)
    res <- utils::stack(rot)
    colnames(res) <- c("value", "component")
    res$component <- as.character(res$component)
    res$terms <- rep(vars, npc)
    res <- as_tibble(res)[, c("terms", "value", "component")]
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl,
                  component  = na_chr)
  }
  res
}
