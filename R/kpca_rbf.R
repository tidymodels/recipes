#' Radial basis function kernel PCA signal extraction
#'
#' `step_kpca_rbf()` creates a *specification* of a recipe step that will
#' convert numeric data into one or more principal components using a radial
#' basis function kernel basis expansion.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param sigma A numeric value for the radial basis function parameter. See the
#'   documentation at [kernlab::rbfdot()].
#' @param res An S4 [kernlab::kpca()] object is stored here once this
#'   preprocessing step has be trained by [prep()].
#' @template step-return
#' @family multivariate transformation steps
#' @export
#' @template kpca-info
#'
#' @details
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_kpca_rbf"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2", "kernlab"))
#' library(ggplot2)
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' kpca_trans <- rec |>
#'   step_YeoJohnson(all_numeric_predictors()) |>
#'   step_normalize(all_numeric_predictors()) |>
#'   step_kpca_rbf(all_numeric_predictors())
#'
#' kpca_estimates <- prep(kpca_trans, training = biomass_tr)
#'
#' kpca_te <- bake(kpca_estimates, biomass_te)
#'
#' ggplot(kpca_te, aes(x = kPC1, y = kPC2)) +
#'   geom_point() +
#'   coord_equal()
#'
#' tidy(kpca_trans, number = 3)
#' tidy(kpca_estimates, number = 3)
step_kpca_rbf <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    num_comp = 5,
    res = NULL,
    columns = NULL,
    sigma = 0.2,
    prefix = "kPC",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("kpca_rbf")
  ) {
    recipes_pkg_check(required_pkgs.step_kpca_rbf())

    add_step(
      recipe,
      step_kpca_rbf_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        res = res,
        columns = columns,
        sigma = sigma,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_kpca_rbf_new <-
  function(
    terms,
    role,
    trained,
    num_comp,
    res,
    columns,
    sigma,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "kpca_rbf",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      res = res,
      columns = columns,
      sigma = sigma,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_kpca_rbf <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$sigma, arg = "sigma", min = .Machine$double.eps)
  check_string(x$prefix, arg = "prefix")
  check_number_whole(x$num_comp, arg = "num_comp", min = 0)

  if (x$num_comp > 0 && length(col_names) > 0) {
    cl <-
      rlang::call2(
        "kpca",
        .ns = "kernlab",
        x = rlang::expr(as.matrix(training[, col_names])),
        features = x$num_comp,
        kernel = "rbfdot",
        kpar = list(sigma = x$sigma)
      )

    kprc <- try_fetch_eval_tidy(rlang::eval_tidy(cl))
  } else {
    kprc <- NULL
  }

  step_kpca_rbf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    sigma = x$sigma,
    res = kprc,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_kpca_rbf <- function(object, new_data, ...) {
  uses_dim_red(object)
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  keep_going <- object$num_comp > 0 && length(col_names) > 0
  if (!keep_going) {
    return(new_data)
  }

  cl <-
    rlang::call2(
      "predict",
      .ns = "kernlab",
      object = object$res,
      rlang::expr(as.matrix(new_data[, col_names]))
    )
  comps <- rlang::eval_tidy(cl)
  comps <- comps[, seq_len(object$num_comp), drop = FALSE]
  colnames(comps) <- names0(ncol(comps), object$prefix)
  comps <- as_tibble(comps)
  comps <- check_name(comps, new_data, object)
  new_data <- vec_cbind(new_data, comps, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_kpca_rbf <- function(x, width = max(20, options()$width - 40), ...) {
  title <- "RBF kernel PCA extraction with "
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_kpca_rbf <- function(x, ...) {
  uses_dim_red(x)
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_kpca_rbf <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "sigma"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "rbf_sigma")
    ),
    source = "recipe",
    component = "step_kpca_rbf",
    component_id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_kpca_rbf <- function(x, ...) {
  c("kernlab")
}
