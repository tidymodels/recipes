#' Impute via k-nearest neighbors
#'
#' `step_impute_knn()` creates a *specification* of a recipe step that will
#' impute missing data using nearest neighbors.
#'
#' @inheritParams step_impute_bag
#' @inheritParams step_center
#' @inheritParams step_pca
#' @param neighbors The number of neighbors.
#' @param options A named list of options to pass to [gower::gower_topn()].
#'   Available options are currently `nthread` and `eps`.
#' @param ref_data A tibble of data that will reflect the data preprocessing
#'   done up to the point of this imputation step. This is `NULL` until the step
#'   is trained by [prep()].
#' @template step-return
#' @family imputation steps
#' @export
#' @details
#'
#' The step uses the training set to impute any other data sets. The only
#' distance function available is Gower's distance which can be used for
#' mixtures of nominal and numeric data.
#'
#' Once the nearest neighbors are determined, the mode is used to predictor
#' nominal variables and the mean is used for numeric data. Note that, if the
#' underlying data are integer, the mean will be converted to an integer too.
#'
#' Note that if a variable that is to be imputed is also in `impute_with`, this
#' variable will be ignored.
#'
#' It is possible that missing values will still occur after imputation if a
#' large majority (or all) of the imputing variables are also missing.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_knnimpute()` to
#' `step_impute_knn()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `predictors`, `neighbors` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{predictors}{character, selected predictors used to impute}
#'   \item{neighbors}{integer, number of neighbors}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_impute_knn"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Gower, C. (1971) "A general coefficient of similarity and some
#'  of its properties," Biometrics, 857-871.
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(biomass, package = "modeldata")
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
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' ratio_recipe <- rec |>
#'   step_impute_knn(all_predictors(), neighbors = 3)
#' ratio_recipe2 <- prep(ratio_recipe, training = biomass_tr)
#' imputed <- bake(ratio_recipe2, biomass_te)
#'
#' # how well did it work?
#' summary(biomass_te_whole$carbon)
#' cbind(
#'   before = biomass_te_whole$carbon[carb_missing],
#'   after = imputed$carbon[carb_missing]
#' )
#'
#' summary(biomass_te_whole$nitrogen)
#' cbind(
#'   before = biomass_te_whole$nitrogen[nitro_missing],
#'   after = imputed$nitrogen[nitro_missing]
#' )
#'
#' tidy(ratio_recipe, number = 1)
#' tidy(ratio_recipe2, number = 1)
step_impute_knn <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    neighbors = 5,
    impute_with = all_predictors(),
    options = list(nthread = 1, eps = 1e-08),
    ref_data = NULL,
    columns = NULL,
    skip = FALSE,
    id = rand_id("impute_knn")
  ) {
    add_step(
      recipe,
      step_impute_knn_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        neighbors = neighbors,
        impute_with = enquos(impute_with),
        ref_data = ref_data,
        options = options,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_impute_knn_new <-
  function(
    terms,
    role,
    trained,
    neighbors,
    impute_with,
    ref_data,
    options,
    columns,
    skip,
    id
  ) {
    step(
      subclass = "impute_knn",
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
prep.step_impute_knn <- function(x, training, info = NULL, ...) {
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)
  check_options(x$options, include = c("nthread", "eps"))

  if (length(x$options) > 0) {
    opt_nms <- names(x$options)
    if (all(opt_nms != "nthread")) {
      x$options$nthread <- 1
    }
    if (all(opt_nms != "eps")) {
      x$options$eps <- 1e-08
    }
  } else {
    x$options <- list(nthread = 1, eps = 1e-08)
  }

  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      training = training,
      info = info
    )
  all_x_vars <- lapply(var_lists, function(x) c(x$x, x$y))
  all_x_vars <- unique(unlist(all_x_vars))

  step_impute_knn_new(
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
  dat <- dat[[names(dat)]]
  dat <- dat[!is.na(dat)]
  est <- if (is.factor(dat) | is.character(dat)) {
    mode_est(dat)
  } else {
    mean(dat)
  }
  est
}

#' @export
bake.step_impute_knn <- function(object, new_data, ...) {
  col_names <- purrr::map_chr(object$columns, "y")
  all_cols <- unique(unlist(object$columns, recursive = TRUE))
  check_new_data(all_cols, object, new_data)

  missing_rows <- !vec_detect_complete(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }

  names(object$columns) <- col_names

  old_data <- new_data
  for (col_name in col_names) {
    missing_rows <- !vec_detect_complete(new_data[, col_name])
    if (!any(missing_rows)) {
      next
    }
    preds <- object$columns[[col_name]]$x
    imp_data <- old_data[missing_rows, preds, drop = FALSE]

    imp_data_all_missing <- vctrs::vec_detect_missing(imp_data)

    if (any(imp_data_all_missing)) {
      offenders <- which(missing_rows)[imp_data_all_missing]
      missing_rows[offenders] <- FALSE

      cli::cli_warn(
        "The {.arg impute_with} variables for {.col {col_name}} only contains
        missing values for row: {offenders}. Cannot impute for those rows.",
      )

      imp_data <- imp_data[!imp_data_all_missing, , drop = FALSE]

      if (nrow(imp_data) == 0) {
        next
      }
    }
    # make sure imp_data as same types as ref_data
    imp_data <- vctrs::tib_cast(
      imp_data,
      select(object$ref_data, names(imp_data))
    )

    imp_var_complete <- !is.na(object$ref_data[[col_name]])
    nn_ind <- nn_index(
      object$ref_data[imp_var_complete, ],
      imp_data,
      preds,
      object$neighbors,
      object$options
    )
    pred_vals <-
      apply(
        nn_ind,
        2,
        nn_pred,
        dat = object$ref_data[imp_var_complete, col_name]
      )
    pred_vals <- cast(pred_vals, object$ref_data[[col_name]])
    new_data[[col_name]] <- vec_cast(new_data[[col_name]], pred_vals)
    new_data[missing_rows, col_name] <- pred_vals
  }
  new_data
}

#' @export
print.step_impute_knn <-
  function(x, width = max(20, options()$width - 31), ...) {
    all_y_vars <- lapply(x$columns, function(x) x$y)
    all_y_vars <- unique(unlist(all_y_vars))
    title <- "K-nearest neighbor imputation for "
    print_step(all_y_vars, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_impute_knn <- function(x, ...) {
  if (is_trained(x)) {
    terms <- purrr::map(x$columns, function(x) unname(x$y))
    predictors <- purrr::map(x$columns, function(x) unname(x$x))
    res <- tibble(terms = terms, predictors = predictors)
    res <- tidyr::unchop(
      data = res,
      cols = tidyselect::all_of(c("terms", "predictors")),
      ptype = list(terms = character(), predictors = character())
    )
    res$neighbors <- rep(x$neighbors, nrow(res))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      predictors = na_chr,
      neighbors = x$neighbors
    )
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_impute_knn <- function(x, ...) {
  tibble::tibble(
    name = "neighbors",
    call_info = list(
      list(pkg = "dials", fun = "neighbors", range = c(1L, 10L))
    ),
    source = "recipe",
    component = "step_impute_knn",
    component_id = x$id
  )
}
