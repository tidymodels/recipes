#' PCA Signal Extraction
#'
#' `step_pca` creates a *specification* of a recipe step that will convert
#'  numeric data into one or more principal components.
#'
#' @inheritParams step_center
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the new columns created by this step from
#'  the original variables will be used as _predictors_ in a model.
#' @param num_comp The number of components to retain as new predictors.
#'  If `num_comp` is greater than the number of columns or the number of
#'  possible components, a smaller value will be used. If `num_comp = 0`
#'  is set then no transformation is done and selected variables will
#'  stay unchanged.
#' @param threshold A fraction of the total variance that should be covered by
#'  the components. For example, `threshold = .75` means that `step_pca` should
#'  generate enough components to capture 75 percent of the variability in the
#'  variables. Note: using this argument will override and reset any value given
#'  to `num_comp`.
#' @param options A list of options to the default method for
#'  [stats::prcomp()]. Argument defaults are set to `retx = FALSE`, `center =
#'  FALSE`, `scale. = FALSE`, and `tol = NULL`. **Note** that the argument `x`
#'  should not be passed here (or at all).
#' @param res The [stats::prcomp.default()] object is stored here once this
#'  preprocessing step has be trained by [prep()].
#' @param columns A character string of variable names that will
#'  be populated elsewhere.
#' @param prefix A character string for the prefix of the resulting new
#'  variables. See notes below.
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `FALSE`.
#' @template step-return
#' @family multivariate transformation steps
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
#' It is advisable to standardize the variables prior to running
#'  PCA. Here, each variable will be centered and scaled prior to
#'  the PCA calculation. This can be changed using the
#'  `options` argument or by using [step_center()]
#'  and [step_scale()].
#'
#' The argument `num_comp` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_comp < 10`, their names will be `PC1` - `PC9`.
#'  If `num_comp = 101`, the names would be `PC001` -
#'  `PC101`.
#'
#' Alternatively, `threshold` can be used to determine the
#'  number of components that are required to capture a specified
#'  fraction of the total variance in the variables.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, use either `type = "coef"`
#' for the variable loadings per component or `type = "variance"` for how
#' much variance each component accounts for.
#'
#' @references Jolliffe, I. T. (2010). *Principal Component
#'  Analysis*. Springer.
#'
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' pca_trans <- rec %>%
#'   step_normalize(all_numeric()) %>%
#'   step_pca(all_numeric(), num_comp = 3)
#' pca_estimates <- prep(pca_trans, training = USArrests)
#' pca_data <- bake(pca_estimates, USArrests)
#'
#' rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
#' plot(pca_data$PC1, pca_data$PC2,
#'      xlim = rng, ylim = rng)
#'
#' with_thresh <- rec %>%
#'   step_normalize(all_numeric()) %>%
#'   step_pca(all_numeric(), threshold = .99)
#' with_thresh <- prep(with_thresh, training = USArrests)
#' bake(with_thresh, USArrests)
#'
#' tidy(pca_trans, number = 2)
#' tidy(pca_estimates, number = 2)
step_pca <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     num_comp  = 5,
                     threshold = NA,
                     options = list(),
                     res = NULL,
                     columns = NULL,
                     prefix = "PC",
                     keep_original_cols = FALSE,
                     skip = FALSE,
                     id = rand_id("pca")) {

  if (!is_tune(threshold) & !is_varying(threshold)) {
    if (!is.na(threshold) && (threshold > 1 | threshold <= 0)) {
      rlang::abort("`threshold` should be on (0, 1].")
    }
  }

  add_step(
    recipe,
    step_pca_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      threshold = threshold,
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

step_pca_new <-
  function(terms, role, trained, num_comp, threshold, options, res, columns,
           prefix,  keep_original_cols, skip, id) {
    step(
      subclass = "pca",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      threshold = threshold,
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
prep.step_pca <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  if (x$num_comp > 0 && length(col_names) > 0) {
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

    x$num_comp <- min(x$num_comp, length(col_names))
    if (!is.na(x$threshold)) {
      total_var <- sum(prc_obj$sdev ^ 2)
      num_comp <-
        which.max(cumsum(prc_obj$sdev ^ 2 / total_var) >= x$threshold)
      if (length(num_comp) == 0)
        num_comp <- length(prc_obj$sdev)
      x$num_comp <- num_comp
    }
    ## decide on removing prc elements that aren't used in new projections
    ## e.g. `sdev` etc.

  } else {
    prc_obj <- NULL
  }

  step_pca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    threshold = x$threshold,
    options = x$options,
    res = prc_obj,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pca <- function(object, new_data, ...) {
  if (length(object$columns) > 0 && !all(is.na(object$res$rotation))) {
    pca_vars <- rownames(object$res$rotation)
    comps <- predict(object$res, newdata = new_data[, pca_vars])
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

print.step_pca <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (x$trained) {
      if (length(x$columns) == 0 || all(is.na(x$res$rotation))) {
        title <- "No PCA components were extracted from "
        columns <- names(x$columns)
      } else {
        title <- glue::glue("PCA extraction with ")
        columns <- rownames(x$res$rotation)
      }
    } else {
      title <- "PCA extraction with "
    }
    print_step(columns, x$terms, x$trained, title, width)
    invisible(x)
  }

pca_coefs <- function(x) {
  if (x$num_comp > 0 && length(x$columns) > 0) {
    rot <- as.data.frame(x$res$rotation)
    npc <- ncol(rot)
    res <- utils::stack(rot)
    colnames(res) <- c("value", "component")
    res$component <- as.character(res$component)
    res$terms <- rep(unname(x$columns), npc)
    res <- as_tibble(res)[, c("terms", "value", "component")]
  } else {
    res <- tibble::tibble(terms = unname(x$columns), value = rlang::na_dbl,
                          component = rlang::na_chr)
  }
  res
}

pca_variances <- function(x) {
  if (x$num_comp > 0 && length(x$columns) > 0) {
    variances <- x$res$sdev ^ 2
    p <- length(variances)
    tot <- sum(variances)
    y <- c(variances,
           cumsum(variances),
           variances / tot * 100,
           cumsum(variances) / tot * 100)
    x <-
      rep(
        c(
          "variance",
          "cumulative variance",
          "percent variance",
          "cumulative percent variance"
        ),
        each = p
      )

    res <- tibble::tibble(terms = x,
                          value = y,
                          component = rep(1:p, 4))
  } else {
    res <- tibble::tibble(
      terms = unname(x$columns),
      value = rep(rlang::na_dbl, length(x$columns)),
      component = rep(rlang::na_chr, length(x$columns))
    )
  }
  res
}



#' @rdname tidy.recipe
#' @param type For `step_pca`, either `"coef"` (for the variable loadings per
#' component) or `"variance"` (how much variance does each component
#' account for).
#' @export
tidy.step_pca <- function(x, type = "coef", ...) {
  if (!is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl,
                  component  = na_chr)
  } else {
    type <- match.arg(type, c("coef", "variance"))
    if (type == "coef") {
      res <- pca_coefs(x)
    } else {
      res <- pca_variances(x)
    }
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_pca <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_pca",
    component_id = x$id
  )
}
