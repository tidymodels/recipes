#' ICA Signal Extraction
#'
#' `step_ica` creates a *specification* of a recipe step
#'  that will convert numeric data into one or more independent
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
#'  that the new independent component columns created by the
#'  original variables will be used as predictors in a model.
#' @param num_comp The number of ICA components to retain as new
#'  predictors. If `num_comp` is greater than the number of columns
#'  or the number of possible components, a smaller value will be
#'  used.
#' @param options A list of options to
#'  [fastICA::fastICA()]. No defaults are set here.
#'  **Note** that the arguments `X` and `n.comp` should
#'  not be passed here.
#' @param res The [fastICA::fastICA()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the loading),
#'  and `component`.
#' @keywords datagen
#' @concept preprocessing
#' @concept ica
#' @concept projection_methods
#' @export
#' @details Independent component analysis (ICA) is a
#'  transformation of a group of variables that produces a new set
#'  of artificial features or components. ICA assumes that the
#'  variables are mixtures of a set of distinct, non-Gaussian
#'  signals and attempts to transform the data to isolate these
#'  signals. Like PCA, the components are statistically independent
#'  from one another. This means that they can be used to combat
#'  large inter-variables correlations in a data set. Also like PCA,
#'  it is advisable to center and scale the variables prior to
#'  running ICA.
#'
#' This package produces components using the "FastICA"
#'  methodology (see reference below). This step requires the
#'  \pkg{dimRed} and \pkg{fastICA} packages. If not installed, the
#'  step will stop with a note about installing these packages.
#'
#' The argument `num_comp` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_comp < 10`, their names will be `IC1` - `IC9`.
#'  If `num_comp = 101`, the names would be `IC001` -
#'  `IC101`.
#'
#' @references Hyvarinen, A., and Oja, E. (2000). Independent
#'  component analysis: algorithms and applications. *Neural
#'  Networks*, 13(4-5), 411-430.
#'
#' @examples
#' # from fastICA::fastICA
#' set.seed(131)
#' S <- matrix(runif(400), 200, 2)
#' A <- matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE)
#' X <- as.data.frame(S %*% A)
#'
#' tr <- X[1:100, ]
#' te <- X[101:200, ]
#'
#' rec <- recipe( ~ ., data = tr)
#'
#' ica_trans <- step_center(rec,  V1, V2)
#' ica_trans <- step_scale(ica_trans, V1, V2)
#' ica_trans <- step_ica(ica_trans, V1, V2, num_comp = 2)
#'
#' if (require(dimRed) & require(fastICA)) {
#'   ica_estimates <- prep(ica_trans, training = tr)
#'   ica_data <- bake(ica_estimates, te)
#'
#'   plot(te$V1, te$V2)
#'   plot(ica_data$IC1, ica_data$IC2)
#'
#'   tidy(ica_trans, number = 3)
#'   tidy(ica_estimates, number = 3)
#' }
#' @seealso [step_pca()] [step_kpca()]
#'   [step_isomap()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_ica <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 5,
           options = list(),
           res = NULL,
           prefix = "IC",
           skip = FALSE,
           id = rand_id("ica")) {


    recipes_pkg_check(c("dimRed", "fastICA"))

    add_step(
      recipe,
      step_ica_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        options = options,
        res = res,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_ica_new <-
  function(terms, role, trained, num_comp, options, res, prefix, skip, id) {
    step(
      subclass = "ica",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_ica <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  if (x$num_comp > 0) {
    x$num_comp <- min(x$num_comp, length(col_names))

    indc <- dimRed::FastICA(stdpars = x$options)
    indc <-
      try(
        indc@fun(
          dimRed::dimRedData(as.data.frame(training[, col_names, drop = FALSE])),
          list(ndim = x$num_comp)
        ),
        silent = TRUE
      )
    if (inherits(indc, "try-error")) {
      stop("`step_ica` failed with error:\n", as.character(indc), call. = FALSE)
    }
  } else {
    indc <- list(x_vars = col_names)
  }

  step_ica_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    options = x$options,
    res = indc,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ica <- function(object, new_data, ...) {
  if (object$num_comp > 0) {
    ica_vars <- colnames(environment(object$res@apply)$indata)
    comps <-
      object$res@apply(
        dimRed::dimRedData(
          as.data.frame(new_data[, ica_vars, drop = FALSE])
        )
      )@data
    comps <- comps[, 1:object$num_comp, drop = FALSE]
    colnames(comps) <- names0(ncol(comps), object$prefix)
    new_data <- bind_cols(new_data, as_tibble(comps))
    new_data <-
      new_data[, !(colnames(new_data) %in% ica_vars), drop = FALSE]
  }
  as_tibble(new_data)
}


print.step_ica <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (x$num_comp == 0) {
      cat("No ICA components were extracted.\n")
    } else {
      cat("ICA extraction with ")
      printer(colnames(x$res@org.data), x$terms, x$trained, width = width)
    }

    invisible(x)
  }

#' @rdname step_ica
#' @param x A `step_ica` object.
#' @export
tidy.step_ica <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      rot <- dimRed::getRotationMatrix(x$res)
      colnames(rot) <- names0(ncol(rot), x$prefix)
      rot <- as.data.frame(rot)
      vars <- colnames(x$res@org.data)
      npc <- ncol(rot)
      res <- utils::stack(rot)
      colnames(res) <- c("value", "component")
      res$component <- as.character(res$component)
      res$terms <- rep(vars, npc)
      res <- as_tibble(res)
    } else {
      res <- tibble(terms = x$res$x_vars, value = na_dbl, component  = na_chr)
    }
  } else {
    term_names <- sel2char(x$terms)
    comp_names <- names0(x$num_comp, x$prefix)
    res <- expand.grid(terms = term_names,
                       value = na_dbl,
                       component  = comp_names)
    res$terms <- as.character(res$terms)
    res$component <- as.character(res$component)
    res <- as_tibble(res)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.step
#' @export
tunable.step_ica <- function(x, ...) {
  tibble::tibble(
    name = "num_comp",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1, 4))),
    source = "recipe",
    component = "step_ica",
    component_id = x$id
  )
}
