#' Partial Least Squares Feature Extraction
#'
#' `step_pls` creates a *specification* of a recipe step that will
#'  convert numeric data into one or more new dimensions.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the dimensions. See
#'  [selections()] for more details. For the `tidy` method, these
#'  are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new dimension columns created by the original variables
#'  will be used as predictors in a model.
#' @param num The number of pls dimensions to retain as new
#'  predictors. If `num` is greater than the number of columns
#'  or the number of possible dimensions, a smaller value will be
#'  used.
#' @param outcome When a single outcome is available, character
#'  string or call to [dplyr::vars()] can be used to specify the
#'  variable. When there are multipole outcomes, [dplyr::vars()]
#'  must be used. This that can include specific variable names
#'  separated by commas or different selectors (see [selections()]).
#' @param options A list of options to [pls::plsr()].
#' @param res The [pls::plsr()] object is stored
#'  here once this preprocessing step has be trained by
#'  [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @keywords datagen
#' @concept preprocessing pls projection_methods
#' @export
#' @details PLS is a supervised version of principal component
#'  analysis that requires one or more numeric outcomes to compute
#'  the new features. The data should be scaled (and perhaps
#'  centered) prior to running these calculations.
#'
#' The argument `num` controls the number of components that will
#'  be retained (the original variables that are used to derive the
#'  components are removed from the data). The new components will
#'  have names that begin with `prefix` and a sequence of numbers.
#'  The variable names are padded with zeros. For example, if `num <
#'  10`, their names will be `PLS1` - `PLS9`. If `num = 101`, the
#'  names would be `PLS001` - `PLS101`.
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' pls_rec <- recipe(HHV ~ ., data = biomass_tr) %>%
#'   step_rm(sample, dataset) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   # If the outcome(s) need standardization, do it in separate
#'   # steps with skip = FALSE so that new data where the
#'   # outcome is missing can be processed.
#'   step_center(all_outcomes(), skip = TRUE) %>%
#'   step_scale(all_outcomes(), skip = TRUE) %>%
#'   step_pls(all_predictors(), outcome = "HHV")
#'
#' pls_rec <- prep(pls_rec, training = biomass_tr, retain = TRUE)
#'
#' pls_test_scores <- bake(pls_rec, newdata = biomass_te[, -8])
#'
#' tidy(pls_rec, number = 6)
#' @seealso [step_pca()] [step_kpca()]
#'   [step_ica()] [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_pls <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num  = 2,
           outcome = NULL,
           options = NULL,
           res = NULL,
           prefix = "PLS",
           skip = FALSE) {
    if (is.null(outcome))
      stop("`outcome` should select at least one column.", call. = FALSE)

    add_step(
      recipe,
      step_pls_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num = num,
        outcome = outcome,
        options = options,
        res = res,
        prefix = prefix,
        skip = skip
      )
    )
  }

step_pls_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           num  = NULL,
           outcome = outcome,
           options = NULL,
           res = NULL,
           prefix = "pls",
           skip = FALSE) {
    step(
      subclass = "pls",
      terms = terms,
      role = role,
      trained = trained,
      num = num,
      outcome = outcome,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip
    )
  }

#' @importFrom pls plsr
#' @export
prep.step_pls <- function(x, training, info = NULL, ...) {
  x_names <- terms_select(x$terms, info = info)
  y_names <- terms_select(x$outcome, info = info)
  check_type(training[, c(y_names, x_names)])

  if(length(y_names) == 1) {
    y_form <- y_names
  } else {
    y_form <- paste0(y_names, collapse = ",")
    y_form <- paste0("cbind(", y_form, ")")
  }
  args <- list(formula = as.formula(paste(y_form, ".", sep = "~")),
               data = training[, c(y_names, x_names)])

  x$options$ncomp <- min(x$num, length(x_names))
  args <- c(args, x$options)
  mod <- do.call(pls::plsr, args)

  if(!any(names(mod) == "scale"))
    mod$scale <- NA

  step_pls_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num = x$num,
    outcome = x$outcome,
    options = x$options,
    res = mod[c("projection", "Xmeans", "scale")],
    prefix = x$prefix,
    skip = x$skip
  )
}

#' @export
bake.step_pls <- function(object, newdata, ...) {
  pls_vars <- rownames(object$res$projection)
  n <- nrow(newdata)
  input_data <- as.matrix(newdata[, pls_vars])

  if(!all(is.na(object$res$scale)))
    input_data <- sweep(input_data, 2, object$res$scale, "/")

  input_data <- sweep(input_data, 2, object$res$Xmeans, "-")

  comps <- input_data %*% object$res$projection
  colnames(comps) <- names0(ncol(comps), object$prefix)
  newdata <- bind_cols(newdata, as_tibble(comps))
  newdata <-
    newdata[, !(colnames(newdata) %in% pls_vars), drop = FALSE]
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}


print.step_pls <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("PLS feature extraction with ")
    printer(rownames(x$res$projection), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_pls
#' @param x A `step_pls` object
tidy.step_pls <- function(x, ...) {
  if (is_trained(x)) {
    res <- as.data.frame(x$res$projection)
    res <- stack(res)
    res$terms <- rep(rownames(x$res$projection), ncol(x$res$projection))
    names(res)[1:2] <- c("value", "component")
    res <- res[, c("terms", "value", "component")]
    res$component <- gsub("Comp ", "PLS", res$component)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = na_chr)
  }
  res
}
