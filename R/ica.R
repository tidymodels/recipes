#' ICA signal extraction
#'
#' `step_ica()` creates a *specification* of a recipe step that will convert
#' numeric data into one or more independent components.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param options A list of options to [fastICA::fastICA()]. No defaults are set
#'   here.
#'  **Note** that the arguments `X` and `n.comp` should
#'   not be passed here.
#' @param seed A single integer to set the random number stream prior to running
#'   ICA.
#' @param res The [fastICA::fastICA()] object is stored here once this
#'   preprocessing step has be trained by [prep()].
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#'
#' Independent component analysis (ICA) is a transformation of a group of
#' variables that produces a new set of artificial features or components. ICA
#' assumes that the variables are mixtures of a set of distinct, non-Gaussian
#' signals and attempts to transform the data to isolate these signals. Like
#' PCA, the components are statistically independent from one another. This
#' means that they can be used to combat large inter-variables correlations in a
#' data set. Also like PCA, it is advisable to center and scale the variables
#' prior to running ICA.
#'
#' This package produces components using the "FastICA" methodology (see
#' reference below). This step requires the \pkg{dimRed} and \pkg{fastICA}
#' packages. If not installed, the step will stop with a note about installing
#' these packages.
#'
#' ```{r, echo = FALSE, results="asis"}
#' prefix <- "IC"
#' result <- knitr::knit_child("man/rmd/num_comp.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `component`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{component}{character, name of component}
#'   \item{value}{numeric, the loading}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_ica"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Hyvarinen, A., and Oja, E. (2000). Independent
#'  component analysis: algorithms and applications. *Neural
#'  Networks*, 13(4-5), 411-430.
#'
#' @examplesIf FALSE
#' # from fastICA::fastICA
#' set.seed(131)
#' S <- matrix(runif(400), 200, 2)
#' A <- matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE)
#' X <- as.data.frame(S %*% A)
#'
#' tr <- X[1:100, ]
#' te <- X[101:200, ]
#'
#' rec <- recipe(~., data = tr)
#'
#' ica_trans <- step_center(rec, V1, V2)
#' ica_trans <- step_scale(ica_trans, V1, V2)
#' ica_trans <- step_ica(ica_trans, V1, V2, num_comp = 2)
#'
#' ica_estimates <- prep(ica_trans, training = tr)
#' ica_data <- bake(ica_estimates, te)
#'
#' plot(te$V1, te$V2)
#' plot(ica_data$IC1, ica_data$IC2)
#'
#' tidy(ica_trans, number = 3)
#' tidy(ica_estimates, number = 3)
step_ica <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    num_comp = 5,
    options = list(method = "C"),
    seed = sample.int(10000, 5),
    res = NULL,
    columns = NULL,
    prefix = "IC",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("ica")
  ) {
    recipes_pkg_check(required_pkgs.step_ica())

    add_step(
      recipe,
      step_ica_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        options = options,
        seed = seed,
        res = res,
        columns = columns,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_ica_new <-
  function(
    terms,
    role,
    trained,
    num_comp,
    options,
    seed,
    res,
    columns,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "ica",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      options = options,
      seed = seed,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_ica <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_string(x$prefix, arg = "prefix")
  check_options(x$options, exclude = c("X", "n.comp"))

  if (x$num_comp > 0 && length(col_names) > 0) {
    x$num_comp <- min(x$num_comp, length(col_names))

    cl <-
      rlang::call2(
        "fastICA",
        .ns = "fastICA",
        n.comp = x$num_comp,
        X = rlang::expr(as.matrix(training[, col_names]))
      )
    cl <- rlang::call_modify(cl, !!!x$options)

    indc <- try_fetch_eval_tidy(withr::with_seed(x$seed, rlang::eval_tidy(cl)))

    indc <- indc[c("K", "W")]
    indc$means <- colMeans(training[, col_names])
  } else {
    indc <- NULL
  }

  step_ica_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    options = x$options,
    seed = x$seed,
    res = indc,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ica <- function(object, new_data, ...) {
  uses_dim_red(object)
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  keep_going <- object$num_comp > 0 && length(col_names) > 0
  if (!keep_going) {
    return(new_data)
  }

  comps <- scale(
    as.matrix(new_data[, col_names]),
    center = object$res$means,
    scale = FALSE
  )
  comps <- comps %*% object$res$K %*% object$res$W
  comps <- comps[, seq_len(object$num_comp), drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  comps <- as_tibble(comps)
  comps <- check_name(comps, new_data, object)
  new_data <- vec_cbind(new_data, comps, .name_repair = "minimal")

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_ica <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (x$num_comp == 0) {
      title <- "No ICA components were extracted from "
    } else {
      title <- "ICA extraction with "
    }

    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_ica <- function(x, ...) {
  uses_dim_red(x)
  if (is_trained(x)) {
    if (x$num_comp > 0 && length(x$columns) > 0) {
      res <- x$res$K %*% x$res$W
      colnames(res) <- names0(ncol(res), x$prefix)
      res <- as.data.frame(res)
      res$terms <- x$columns
      res <-
        tidyr::pivot_longer(
          res,
          cols = dplyr::starts_with(x$prefix),
          names_to = "component",
          values_to = "value"
        )
    } else {
      res <-
        tibble(
          terms = unname(x$columns),
          value = na_dbl,
          component = na_chr
        )
    }
  } else {
    term_names <- sel2char(x$terms)
    comp_names <- names0(x$num_comp, x$prefix)
    res <- tidyr::crossing(
      terms = term_names,
      value = na_dbl,
      component = comp_names
    )
    res$terms <- as.character(res$terms)
    res$component <- as.character(res$component)
    res <- as_tibble(res)
  }

  res$id <- x$id
  res <- dplyr::arrange(res, terms, component)

  dplyr::select(res, terms, component, value, id)
}

#' @export
tunable.step_ica <- function(x, ...) {
  tibble::tibble(
    name = "num_comp",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1L, 4L))),
    source = "recipe",
    component = "step_ica",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_ica <- function(x, ...) {
  c("fastICA")
}
