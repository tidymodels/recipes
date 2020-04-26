#' Imputation via K-Nearest Neighbors
#'
#' `step_knnimpute` creates a *specification* of a recipe step that will
#'  impute missing data using nearest neighbors.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose variables. For
#'  `step_knnimpute`, this indicates the variables to be imputed. When used with
#'  `imp_vars`, the dots indicates which variables are used to predict the
#'  missing data in each variable. See [selections()] for more details. For the
#'  `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param impute_with A call to `imp_vars` to specify which variables are used
#'  to impute the variables that can include specific variable names separated
#'  by commas or different selectors (see [selections()]). If a column is
#'  included in both lists to be imputed and to be an imputation predictor, it
#'  will be removed from the latter and not used to impute itself.
#' @param neighbors The number of neighbors.
#' @param options A named list of options to pass to [gower::gower_topn()].
#'  Available options are currently `nthread` and `eps`.
#' @param ref_data A tibble of data that will reflect the data preprocessing
#'  done up to the point of this imputation step. This is `NULL` until the step
#'  is trained by [prep.recipe()].
#' @param columns The column names that will be imputed and used for
#'  imputation. This is `NULL` until the step is trained by [prep.recipe()].
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables for imputation), `predictors`
#'  (those variables used to impute), and `neighbors`.
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details The step uses the training set to impute any other data sets. The
#'  only distance function available is Gower's distance which can be used for
#'  mixtures of nominal and numeric data.
#'
#' Once the nearest neighbors are determined, the mode is used to predictor
#'  nominal variables and the mean is used for numeric data. Note that, if the
#'  underlying data are integer, the mean will be converted to an integer too.
#'
#' Note that if a variable that is to be imputed is also in `impute_with`,
#'  this variable will be ignored.
#'
#' It is possible that missing values will still occur after imputation if a
#'  large majority (or all) of the imputing variables are also missing.
#'
#' @references Gower, C. (1971) "A general coefficient of similarity and some
#'  of its properties," Biometrics, 857-871.
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#' biomass_te_whole <- biomass_te
#'
#' # induce some missing data at random
#' set.seed(9039)
#' carb_missing <- sample(1:nrow(biomass_te), 3)
#' nitro_missing <- sample(1:nrow(biomass_te), 3)
#'
#' biomass_te$carbon[carb_missing] <- NA
#' biomass_te$nitrogen[nitro_missing] <- NA
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' ratio_recipe <- rec %>%
#'   step_knnimpute(all_predictors(), neighbors = 3)
#' ratio_recipe2 <- prep(ratio_recipe, training = biomass_tr)
#' imputed <- bake(ratio_recipe2, biomass_te)
#'
#' # how well did it work?
#' summary(biomass_te_whole$carbon)
#' cbind(before = biomass_te_whole$carbon[carb_missing],
#'       after = imputed$carbon[carb_missing])
#'
#' summary(biomass_te_whole$nitrogen)
#' cbind(before = biomass_te_whole$nitrogen[nitro_missing],
#'       after = imputed$nitrogen[nitro_missing])
#'
#' tidy(ratio_recipe, number = 1)
#' tidy(ratio_recipe2, number = 1)

step_knnimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           neighbors = 5,
           impute_with = imp_vars(all_predictors()),
           options = list(nthread = 1, eps = 1e-08),
           ref_data = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("knnimpute")) {
    if (is.null(impute_with)) {
      rlang::abort("Please list some variables in `impute_with`")
    }

    if (!is.list(options))
      rlang::abort("`options` should be a named list.")
    opt_nms <- names(options)
    if (length(options) > 0) {
      if (any(!(opt_nms %in% c("eps", "nthread")))) {
        rlang::abort("Availible options are 'eps', and 'nthread'.")
      }
      if (all(opt_nms != "nthread")) {
        options$nthread <- 1
      }
      if (all(opt_nms != "eps")) {
        options$eps <- 1e-08
      }
    } else {
      options <- list(nthread = 1, eps = 1e-08)
    }

    add_step(
      recipe,
      step_knnimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        neighbors = neighbors,
        impute_with = impute_with,
        ref_data = ref_data,
        options = options,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_knnimpute_new <-
  function(terms, role, trained, neighbors, impute_with, ref_data, options,
           columns, skip, id) {
    step(
      subclass = "knnimpute",
      terms = terms,
      role = role,
      trained = trained,
      neighbors = neighbors,
      impute_with = impute_with,
      ref_data = ref_data,
      options = options,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_knnimpute <- function(x, training, info = NULL, ...) {
  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      info = info
    )
  all_x_vars <- lapply(var_lists, function(x) c(x$x, x$y))
  all_x_vars <- unique(unlist(all_x_vars))

  step_knnimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    neighbors = x$neighbors,
    impute_with = x$impute_with,
    ref_data = training[, all_x_vars],
    options = x$options,
    columns = var_lists,
    skip = x$skip,
    id = x$id
  )
}

nn_index <- function(miss_data, ref_data, vars, K, opt) {
  gower_topn(
    ref_data[, vars],
    miss_data[, vars],
    n = K,
    nthread = opt$nthread,
    eps = opt$eps
  )$index
}

nn_pred <- function(index, dat) {
  dat <- dat[index, ]
  dat <- getElement(dat, names(dat))
  dat <- dat[!is.na(dat)]
  est <- if (is.factor(dat) | is.character(dat))
    mode_est(dat)
  else
    mean(dat)
  est
}


#' @export
bake.step_knnimpute <- function(object, new_data, ...) {
  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows))
    return(new_data)

  old_data <- new_data
  for (i in seq(along = object$columns)) {
    imp_var <- object$columns[[i]]$y
    missing_rows <- !complete.cases(new_data[, imp_var])
    if (any(missing_rows)) {
      preds <- object$columns[[i]]$x
      imp_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if (all(is.na(imp_data))) {
        rlang::warn("All predictors are missing; cannot impute")
      } else {
        imp_var_complete <- !is.na(object$ref_data[[imp_var]])
        nn_ind <- nn_index(object$ref_data[imp_var_complete,],
                           imp_data, preds,
                           object$neighbors,
                           object$options)
        pred_vals <-
          apply(nn_ind, 2, nn_pred, dat = object$ref_data[imp_var_complete, imp_var])
        pred_vals <- cast(pred_vals, object$ref_data[[imp_var]])
        new_data[missing_rows, imp_var] <- pred_vals
      }
    }
  }
  new_data
}


print.step_knnimpute <-
  function(x, width = max(20, options()$width - 31), ...) {
    all_x_vars <- lapply(x$columns, function(x) x$x)
    all_x_vars <- unique(unlist(all_x_vars))
    cat("K-nearest neighbor imputation for ", sep = "")
    printer(all_x_vars, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_knnimpute
#' @param x A `step_knnimpute` object.
#' @export
tidy.step_knnimpute <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_df(x$columns,
                         function(x)
                           data.frame(
                             terms = x$y,
                             predictors = x$x,
                             stringsAsFactors = FALSE
                           )
    )
    res <- as_tibble(res)
    res$neighbors <- rep(x$neighbors, nrow(res))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, predictors = na_chr, neighbors = x$neighbors)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.step
#' @export
tunable.step_knnimpute <- function(x, ...) {
  tibble::tibble(
    name = "neighbors",
    call_info = list(list(pkg = "dials", fun = "neighbors", range = c(1L, 10L))),
    source = "recipe",
    component = "step_knnimpute",
    component_id = x$id
  )
}
