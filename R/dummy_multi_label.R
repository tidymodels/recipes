#' dummy_multi_label Signal Extraction
#'
#' `step_dummy_multi_label` creates a *specification* of a recipe step that will convert
#'  numeric data into one or more principal components.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'  used to compute the components. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that the new principal
#'  component columns created by the original variables will be used as
#'  predictors in a model.
#' @param num_comp The number of dummy_multi_label components to retain as new predictors.
#'  If `num_comp` is greater than the number of columns or the number of
#'  possible components, a smaller value will be used.
#' @param threshold A fraction of the total variance that should be covered by
#'  the components. For example, `threshold = .75` means that `step_dummy_multi_label` should
#'  generate enough components to capture 75 percent of the variability in the
#'  variables. Note: using this argument will override and reset any value given
#'  to `num_comp`.
#' @param options A list of options to the default method for
#'  [stats::prcomp()]. Argument defaults are set to `retx = FALSE`, `center =
#'  FALSE`, `scale. = FALSE`, and `tol = NULL`. **Note** that the argument `x`
#'  should not be passed here (or at all).
#' @param res The [stats::prcomp.default()] object is stored here once this
#'  preprocessing step has be trained by [prep.recipe()].
#' @param prefix A character string that will be the prefix to the resulting
#'  new variables. See notes below.
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any).
#' @keywords datagen
#' @concept preprocessing
#' @concept dummy_multi_label
#' @concept projection_methods
#' @export
#' @details
#' Principal component analysis (dummy_multi_label) is a transformation of a
#'  group of variables that produces a new set of artificial
#'  features or components. These components are designed to capture
#'  the maximum amount of information (i.e. variance) in the
#'  original variables. Also, the components are statistically
#'  independent from one another. This means that they can be used
#'  to combat large inter-variables correlations in a data set.
#'
#' It is advisable to standardize the variables prior to running
#'  dummy_multi_label. Here, each variable will be centered and scaled prior to
#'  the dummy_multi_label calculation. This can be changed using the
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
#' When you [`tidy()`] this step, use either `type = "coef"` for the variable
#'  loadings per component or `type = "variance"` for how much variance each
#'  component accounts for.
#'
#' @references Jolliffe, I. T. (2010). *Principal Component
#'  Analysis*. Springer.
#'
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' dummy_multi_label_trans <- rec %>%
#'   step_normalize(all_numeric()) %>%
#'   step_dummy_multi_label(all_numeric(), num_comp = 3)
#' dummy_multi_label_estimates <- prep(dummy_multi_label_trans, training = USArrests)
#' dummy_multi_label_data <- bake(dummy_multi_label_estimates, USArrests)
#'
#' rng <- extendrange(c(dummy_multi_label_data$PC1, dummy_multi_label_data$PC2))
#' plot(dummy_multi_label_data$PC1, dummy_multi_label_data$PC2,
#'      xlim = rng, ylim = rng)
#'
#' with_thresh <- rec %>%
#'   step_normalize(all_numeric()) %>%
#'   step_dummy_multi_label(all_numeric(), threshold = .99)
#' with_thresh <- prep(with_thresh, training = USArrests)
#' bake(with_thresh, USArrests)
#'
#' tidy(dummy_multi_label_trans, number = 2)
#' tidy(dummy_multi_label_estimates, number = 2)
#' @seealso [step_ica()] [step_kpca()]
#'   [step_isomap()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_dummy_multi_label <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     threshold = NA,
                     res = NULL,
                     input = NULL,
                     prefix = NULL,
                     keep_original_cols = FALSE,
                     skip = FALSE,
                     id = rand_id("dummy_multi_label")) {

  if (!is_tune(threshold) & !is_varying(threshold)) {
    if (threshold <= 0) {
      rlang::abort("`threshold` should be greater than zero")
    }
    if (threshold >= 1 && !is_integerish(threshold)) {
      rlang::abort("If `threshold` is greater than one it should be an integer.")
    }
  }

  add_step(
    recipe,
    step_dummy_multi_label_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      threshold = threshold,
      res = res,
      input = input,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_dummy_multi_label_new <-
  function(terms, role, trained, threshold, res, input,
           prefix,  keep_original_cols, skip, id) {
    step(
      subclass = "dummy_multi_label",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      res = res,
      input = input,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_multi_label <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  if (length(col_names) > 0) {
    fac_check <- vapply(training[, col_names], is.factor, logical(1))

    #col_names <- col_names[fac_check]
    if (length(col_names) == 0) {
      rlang::abort(
        paste0(
          "The `terms` argument in `step_dummy` did not select ",
          "any factor columns."
        )
      )
    }
  }

  my_levels <- unique(unlist(purrr::map(training[, col_names], levels), use.names = FALSE))

  step_dummy_multi_label_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    res = my_levels,
    input = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dummy_multi_label <- function(object, new_data, ...) {

  col_names <- object$input

  new_columns <- multi_dummy(new_data[, col_names])

  new_data <- bind_cols(new_data, as_tibble(new_columns))
  keep_original_cols <- get_keep_original_cols(object)

  if (!keep_original_cols) {
    new_data <- new_data[, !(colnames(new_data) %in% col_names), drop = FALSE]
  }

  as_tibble(new_data)
}

print.step_dummy_multi_label <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (all(is.na(x$res$rotation))) {
      cat("No PCA components were extracted.\n")
    } else {
      cat("PCA extraction with ")
      #printer(rownames(x$res$rotation), x$terms, x$trained, width = width)
    }

    invisible(x)
  }

multi_dummy <- function(x) {
  row_id <- rep(seq_len(nrow(x)), times = ncol(x))
  values <- unlist(purrr::map(x, as.character), use.names = FALSE)

  row_id <- row_id[!is.na(values)]
  values <- values[!is.na(values)]

  values <- factor(values)

  res <- Matrix::sparseMatrix(
    i = row_id,
    j = as.numeric(values),
    dims = c(nrow(x), length(levels(values)))
  )

  colnames(res) <- levels(values)

  as.matrix(res) %>%
    as_tibble() %>%
    mutate_all(as.integer)
}


#' @rdname tidy.recipe
#' @param x A `step_dummy_multi_label` object.
#' @param type For `step_dummy_multi_label`, either "coef" (for the variable
#'  loadings per component) or "variance" (how much variance does each component
#'  account for).
#' @export
tidy.step_dummy_multi_label <- function(x, type = "coef", ...) {
  if (!is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl,
                  component  = na_chr)
  } else {
    type <- match.arg(type, c("coef", "variance"))
    if (type == "coef") {
      res <- dummy_multi_label_coefs(x)
    } else {
      res <- dummy_multi_label_variances(x)
    }
  }
  res$id <- x$id
  res
}



#' @rdname tunable.step
#' @export
tunable.step_dummy_multi_label <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_dummy_multi_label",
    component_id = x$id
  )
}
