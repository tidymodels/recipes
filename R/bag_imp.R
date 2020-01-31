#' Imputation via Bagged Trees
#'
#' `step_bagimpute` creates a *specification* of a recipe step that will
#'  create bagged tree models to impute missing data.

#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose variables. For
#'  `step_bagimpute`, this indicates the variables to be imputed. When used with
#'  `imp_vars`, the dots indicates which variables are used to predict the
#'  missing data in each variable. See [selections()] for more details. For the
#'  `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are created.

#' @param impute_with A call to `imp_vars` to specify which variables are used
#'  to impute the variables that can include specific variable names separated
#'  by commas or different selectors (see [selections()]). If a column is
#'  included in both lists to be imputed and to be an imputation predictor, it
#'  will be removed from the latter and not used to impute itself.
#' @param trees An integer for the number bagged trees to use in each model.
#' @param options A list of options to [ipred::ipredbagg()]. Defaults are set
#'  for the arguments `nbagg` and `keepX` but others can be passed in. **Note**
#'  that the arguments `X` and `y` should not be passed here.
#' @param seed_val A integer used to create reproducible models. The same seed
#'  is used across all imputation models.
#' @param models The [ipred::ipredbagg()] objects are stored here once this
#'  bagged trees have be trained by [prep.recipe()].
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  bagged tree object).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details For each variables requiring imputation, a bagged tree is created
#'  where the outcome is the variable of interest and the predictors are any
#'  other variables listed in the `impute_with` formula. One advantage to the
#'  bagged tree is that is can accept predictors that have missing values
#'  themselves. This imputation method can be used when the variable of interest
#'  (and predictors) are numeric or categorical. Imputed categorical variables
#'  will remain categorical. Also, integers will be imputed to integer too.
#'
#'   Note that if a variable that is to be imputed is also in `impute_with`,
#'  this variable will be ignored.
#'
#'   It is possible that missing values will still occur after imputation if a
#'  large majority (or all) of the imputing variables are also missing.
#' @references Kuhn, M. and Johnson, K. (2013). *Applied Predictive Modeling*.
#'  Springer Verlag.
#' @examples
#' library(modeldata)
#' data("credit_data")
#'
#' ## missing data per column
#' vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
#'
#' set.seed(342)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[ in_training, ]
#' credit_te <- credit_data[-in_training, ]
#' missing_examples <- c(14, 394, 565)
#'
#' rec <- recipe(Price ~ ., data = credit_tr)
#' \dontrun{
#' impute_rec <- rec %>%
#'   step_bagimpute(Status, Home, Marital, Job, Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples,]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#'
#' ## Specifying which variables to imputate with
#'
#'  impute_rec <- rec %>%
#'   step_bagimpute(Status, Home, Marital, Job, Income, Assets, Debt,
#'                  impute_with = imp_vars(Time, Age, Expenses),
#'                  # for quick execution, nbagg lowered
#'                  options = list(nbagg = 5, keepX = FALSE))
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples,]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#' }

step_bagimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           impute_with = imp_vars(all_predictors()),
           trees = 25,
           models = NULL,
           options = list(keepX = FALSE),
           seed_val = sample.int(10 ^ 4, 1),
           skip = FALSE,
           id = rand_id("bagimpute")) {
    if (is.null(impute_with))
      rlang::abort("Please list some variables in `impute_with`")
    add_step(
      recipe,
      step_bagimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        impute_with = impute_with,
        trees = trees,
        models = models,
        options = options,
        seed_val = seed_val,
        skip = skip,
        id = id
      )
    )
  }

step_bagimpute_new <-
  function(terms, role, trained, models, options, impute_with, trees,
           seed_val, skip, id) {
    step(
      subclass = "bagimpute",
      terms = terms,
      role = role,
      trained = trained,
      impute_with = impute_with,
      trees = trees,
      models = models,
      options = options,
      seed_val = seed_val,
      skip = skip,
      id = id
    )
  }


bag_wrap <- function(vars, dat, opt, seed_val) {
  seed_val <- seed_val[1]
  dat <- as.data.frame(dat[, c(vars$y, vars$x)])
  if (!is.null(seed_val) && !is.na(seed_val))
    set.seed(seed_val)

  out <- do.call("ipredbagg",
                 c(list(y = dat[, vars$y],
                        X = dat[, vars$x, drop = FALSE]),
                   opt))
  out$..imp_vars <- vars$x
  out
}

## This figures out which data should be used to predict each variable
## scheduled for imputation
impute_var_lists <- function(to_impute, impute_using, info) {
  to_impute <- terms_select(terms = to_impute, info = info)
  impute_using <- terms_select(terms = impute_using, info = info)
  var_lists <- vector(mode = "list", length = length(to_impute))
  for (i in seq_along(var_lists)) {
    var_lists[[i]] <- list(y = to_impute[i],
                           x = impute_using[!(impute_using %in% to_impute[i])])
  }
  var_lists
}

#' @export
prep.step_bagimpute <- function(x, training, info = NULL, ...) {
  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      info = info
    )
  opt <- x$options
  opt$nbagg <- x$trees

  x$models <- lapply(
    var_lists,
    bag_wrap,
    dat = training,
    opt = opt,
    seed_val = x$seed_val
  )
  names(x$models) <- vapply(var_lists, function(x) x$y, c(""))

  step_bagimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    models = x$models,
    options = x$options,
    impute_with = x$impute_with,
    trees = x$trees,
    seed_val = x$seed_val,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_bagimpute <- function(object, new_data, ...) {
  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows))
    return(new_data)

  old_data <- new_data
  for (i in seq(along = object$models)) {
    imp_var <- names(object$models)[i]
    missing_rows <- !complete.cases(new_data[, imp_var])
    if (any(missing_rows)) {
      preds <- object$models[[imp_var]]$..imp_vars
      pred_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if (all(is.na(pred_data))) {
        rlang::warn("All predictors are missing; cannot impute")
      } else {
        pred_vals <- predict(object$models[[imp_var]], pred_data)
        pred_vals <- cast(pred_vals, new_data[[imp_var]])
        new_data[missing_rows, imp_var] <- pred_vals
      }
    }
  }
  ## changes character to factor!
  as_tibble(new_data)
}


print.step_bagimpute <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Bagged tree imputation for ", sep = "")
    printer(names(x$models), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
#' @rdname step_bagimpute
imp_vars <- function(...) quos(...)

#' @rdname step_bagimpute
#' @param x A `step_bagimpute` object.
#' @export
tidy.step_bagimpute <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$models),
                  model = x$models)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = NA)
  }
  res$id <- x$id
  res
}


# ------------------------------------------------------------------------------

#' @rdname tunable.step
#' @export
tunable.step_bagimpute <- function(x, ...) {
  tibble::tibble(
    name = "trees",
    call_info = list(list(pkg = "dials", fun = "trees", range = c(5, 25))),
    source = "recipe",
    component = "step_bagimpute",
    component_id = x$id
  )
}
