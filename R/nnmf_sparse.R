#' Non-negative matrix factorization signal extraction with lasso penalization
#'
#' `step_nnmf_sparse()` creates a *specification* of a recipe step that will
#' convert numeric data into one or more non-negative components.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param penalty A non-negative number used as a penalization factor for the
#'   loadings. Values are usually between zero and one.
#' @param options A list of options to `nmf()` in the RcppML package. That
#'   package has a separate function `setRcppMLthreads()` that controls the
#'   amount of internal parallelization. **Note** that the argument `A`, `k`,
#'   `L1`, and `seed` should not be passed here.
#' @param res A matrix of loadings is stored here, along with the names of the
#'   original predictors, once this preprocessing step has been trained by
#'   [prep()].
#' @param seed An integer that will be used to set the seed in isolation when
#'   computing the factorization.
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @details
#'
#' Non-negative matrix factorization computes latent components that have
#' non-negative values and take into account that the original data have
#' non-negative values.
#'
#' ```{r, echo = FALSE, results="asis"}
#' prefix <- "NNMF"
#' result <- knitr::knit_child("man/rmd/num_comp.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, `component` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, value of loading}
#'   \item{component}{character, name of component}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_nnmf_sparse"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @examplesIf .Platform$OS.type!= "windows"
#' if (rlang::is_installed(c("modeldata", "RcppML", "ggplot2"))) {
#' library(Matrix)
#' data(biomass, package = "modeldata")
#'
#' rec <- recipe(HHV ~ ., data = biomass) |>
#'   update_role(sample, new_role = "id var") |>
#'   update_role(dataset, new_role = "split variable") |>
#'   step_nnmf_sparse(
#'     all_numeric_predictors(),
#'     num_comp = 2,
#'     seed = 473,
#'     penalty = 0.01
#'   ) |>
#'   prep(training = biomass)
#'
#' bake(rec, new_data = NULL)
#'
#' library(ggplot2)
#' bake(rec, new_data = NULL) |>
#'   ggplot(aes(x = NNMF2, y = NNMF1, col = HHV)) +
#'   geom_point()
#' }
step_nnmf_sparse <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    num_comp = 2,
    penalty = 0.001,
    options = list(),
    res = NULL,
    prefix = "NNMF",
    seed = sample.int(10^5, 1),
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("nnmf_sparse")
  ) {
    recipes_pkg_check(required_pkgs.step_nnmf_sparse())
    add_step(
      recipe,
      step_nnmf_sparse_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        penalty = penalty,
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

step_nnmf_sparse_new <-
  function(
    terms,
    role,
    trained,
    num_comp,
    penalty,
    options,
    res,
    prefix,
    seed,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "nnmf_sparse",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      penalty = penalty,
      options = options,
      res = res,
      prefix = prefix,
      seed = seed,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

tibble_to_sparse <- function(x, transp = FALSE) {
  x <- as.matrix(x)
  if (transp) {
    x <- t(x)
  }
  Matrix::Matrix(x, sparse = TRUE)
}

nnmf_pen_call <- function(x) {
  opts <-
    list(
      A = expr(dat),
      k = x$num_comp,
      L1 = c(x$penalty, x$penalty),
      verbose = FALSE,
      seed = x$seed,
      nonneg = TRUE
    )
  cl <- rlang::call2("nmf", .ns = "RcppML", !!!opts)
  user_opts <- x$opt
  cl <- rlang::call_modify(cl, !!!user_opts)
  cl
}

#' @export
prep.step_nnmf_sparse <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_whole(x$num_comp, arg = "num_comp", min = 0)
  check_number_decimal(x$penalty, arg = "penalty", min = .Machine$double.eps)
  check_string(x$prefix, arg = "prefix")
  check_options(x$options, exclude = c("A", "k", "L1", "seed"))

  if (x$num_comp > 0 && length(col_names) > 0) {
    x$num_comp <- min(x$num_comp, length(col_names))
    dat <- tibble_to_sparse(training[, col_names], transp = TRUE)
    cl <- nnmf_pen_call(x)

    if (!"package:Matrix" %in% search()) {
      attachNamespace("Matrix")
    }

    nnm <- try_fetch_eval_tidy(rlang::eval_tidy(cl))

    na_w <- sum(is.na(nnm$w))
    if (na_w > 0) {
      cli::cli_abort(
        c(
          x = "The NNMF loadings are missing.",
          i = "The penalty may have been too high or missing values are present in data."
        )
      )
    } else {
      nnm <- list(x_vars = col_names, w = nnm$w)
      rownames(nnm$w) <- col_names
      colnames(nnm$w) <- names0(ncol(nnm$w), x$prefix)
    }
  } else {
    nnm <- list(x_vars = col_names, w = NULL)
    x$num_comp <- 0
  }

  step_nnmf_sparse_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    penalty = x$penalty,
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
bake.step_nnmf_sparse <- function(object, new_data, ...) {
  check_new_data(object$res$x_vars, object, new_data)

  if (object$num_comp == 0) {
    return(new_data)
  }

  proj_data <- as.matrix(new_data[, object$res$x_vars, drop = FALSE])
  proj_data <- proj_data %*% object$res$w
  colnames(proj_data) <- names0(ncol(proj_data), object$prefix)
  proj_data <- as_tibble(proj_data)
  proj_data <- check_name(proj_data, new_data, object)
  new_data <- vec_cbind(new_data, proj_data, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, object$res$x_vars)
  new_data
}

#' @export
print.step_nnmf_sparse <- function(
  x,
  width = max(20, options()$width - 29),
  ...
) {
  if (x$trained) {
    if (x$num_comp == 0) {
      title <- "No non-negative matrix factorization was extracted from "
    } else {
      title <- "Non-negative matrix factorization for "
    }
    columns <- names(x$res$x_vars)
  } else {
    title <- "Non-negative matrix factorization for "
  }
  print_step(columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @rdname tidy.recipe
#' @param x A `step_nnmf_sparse` object.
tidy.step_nnmf_sparse <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- x$res$w

      var_nms <- rownames(res)
      res <- tibble::as_tibble(res)
      res$terms <- var_nms
      res <- tidyr::pivot_longer(
        res,
        cols = c(-terms),
        names_to = "component",
        values_to = "value"
      )
      res <- res[, c("terms", "value", "component")]
      res <- res[order(res$component, res$terms), ]
    } else {
      res <- tibble(
        terms = unname(x$res$x_vars),
        value = na_dbl,
        component = na_chr
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = na_chr)
  }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------

#' @export
tunable.step_nnmf_sparse <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "penalty"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "penalty")
    ),
    source = "recipe",
    component = "step_nnmf_sparse",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_nnmf_sparse <- function(x, ...) {
  c("Matrix", "RcppML")
}
