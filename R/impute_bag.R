#' Impute via bagged trees
#'
#' `step_impute_bag()` creates a *specification* of a recipe step that will
#' create bagged tree models to impute missing data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables to be imputed.
#'  When used with `imp_vars`, these dots indicate which variables are used to
#'  predict the missing data in each variable. See [selections()] for more
#'  details.
#' @param impute_with A call to `imp_vars` to specify which variables are used
#'  to impute the variables that can include specific variable names separated
#'  by commas or different selectors (see [selections()]). If a column is
#'  included in both lists to be imputed and to be an imputation predictor, it
#'  will be removed from the latter and not used to impute itself.
#' @param trees An integer for the number of bagged trees to use in each model.
#' @param options A list of options to [ipred::ipredbagg()]. Defaults are set
#'  for the arguments `nbagg` and `keepX` but others can be passed in. **Note**
#'  that the arguments `X` and `y` should not be passed here.
#' @param seed_val An integer used to create reproducible models. The same seed
#'  is used across all imputation models.
#' @param models The [ipred::ipredbagg()] objects are stored here once this
#'  bagged trees have be trained by [prep()].
#' @template step-return
#' @family imputation steps
#' @export
#' @details For each variable requiring imputation, a bagged tree is created
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
#'
#'  As of `recipes` 0.1.16, this function name changed from `step_bagimpute()`
#'    to `step_impute_bag()`.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or variables selected) and `model`
#'  (the bagged tree object) is returned.
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_impute_bag"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Kuhn, M. and Johnson, K. (2013). *Applied Predictive Modeling*.
#'  Springer Verlag.
#' @examplesIf rlang::is_installed("modeldata")
#' data("credit_data", package = "modeldata")
#'
#' ## missing data per column
#' vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
#'
#' set.seed(342)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[in_training, ]
#' credit_te <- credit_data[-in_training, ]
#' missing_examples <- c(14, 394, 565)
#'
#' rec <- recipe(Price ~ ., data = credit_tr)
#' \dontrun{
#' impute_rec <- rec %>%
#'   step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#'
#' ## Specifying which variables to imputate with
#'
#' impute_rec <- rec %>%
#'   step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt,
#'     impute_with = imp_vars(Time, Age, Expenses),
#'     # for quick execution, nbagg lowered
#'     options = list(nbagg = 5, keepX = FALSE)
#'   )
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te, everything())
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#' }
step_impute_bag <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           impute_with = imp_vars(all_predictors()),
           trees = 25,
           models = NULL,
           options = list(keepX = FALSE),
           seed_val = sample.int(10^4, 1),
           skip = FALSE,
           id = rand_id("impute_bag")) {
    if (is.null(impute_with)) {
      rlang::abort("Please list some variables in `impute_with`")
    }
    add_step(
      recipe,
      step_impute_bag_new(
        terms = enquos(...),
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

#' @rdname step_impute_bag
#' @export
step_bagimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           impute_with = imp_vars(all_predictors()),
           trees = 25,
           models = NULL,
           options = list(keepX = FALSE),
           seed_val = sample.int(10^4, 1),
           skip = FALSE,
           id = rand_id("impute_bag")) {
    lifecycle::deprecate_stop(
      when = "0.1.16",
      what = "recipes::step_bagimpute()",
      with = "recipes::step_impute_bag()"
    )
    step_impute_bag(
      recipe,
      ...,
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

step_impute_bag_new <-
  function(terms, role, trained, models, options, impute_with, trees,
           seed_val, skip, id) {
    step(
      subclass = "impute_bag",
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
  if (is.character(dat[[vars$y]])) {
    dat[[vars$y]] <- factor(dat[[vars$y]])
  }

  if (!is.null(seed_val) && !is.na(seed_val)) {
    set.seed(seed_val)
  }

  out <- do.call(
    "ipredbagg",
    c(
      list(
        y = dat[, vars$y],
        X = dat[, vars$x, drop = FALSE]
      ),
      opt
    )
  )
  out$..imp_vars <- vars$x
  out
}

## This figures out which data should be used to predict each variable
## scheduled for imputation
impute_var_lists <- function(to_impute, impute_using, training, info) {
  to_impute <- recipes_eval_select(to_impute, training, info)
  impute_using <- recipes_eval_select(impute_using, training, info)

  var_lists <- vector(mode = "list", length = length(to_impute))
  for (i in seq_along(var_lists)) {
    var_lists[[i]] <- list(
      y = to_impute[i],
      x = impute_using[!(impute_using %in% to_impute[i])]
    )
  }
  var_lists
}

#' @export
prep.step_impute_bag <- function(x, training, info = NULL, ...) {
  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      training = training,
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

  step_impute_bag_new(
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
#' @keywords internal
prep.step_bagimpute <- prep.step_impute_bag

#' @export
bake.step_impute_bag <- function(object, new_data, ...) {
  col_names <- names(object$models)
  check_new_data(col_names, object, new_data)

  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }

  old_data <- new_data
  for (col_name in col_names) {
    missing_rows <- !complete.cases(new_data[[col_name]])
    if (!any(missing_rows)) {
      next
    }
    preds <- object$models[[col_name]]$..imp_vars
    pred_data <- old_data[missing_rows, preds, drop = FALSE]
    ## do a better job of checking this:
    if (all(is.na(pred_data))) {
      rlang::warn("All predictors are missing; cannot impute")
    } else {
      pred_vals <- predict(object$models[[col_name]], pred_data)
      # For an ipred bug reported on 2021-09-14:
      pred_vals <- cast(pred_vals, object$models[[col_name]]$y)
      new_data[missing_rows, col_name] <- pred_vals
    }
  }
  new_data
}

#' @export
#' @keywords internal
bake.step_bagimpute <- bake.step_impute_bag

#' @export
print.step_impute_bag <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Bagged tree imputation for "
    print_step(names(x$models), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @export
#' @keywords internal
print.step_bagimpute <- print.step_impute_bag

#' @export
#' @rdname step_impute_bag
imp_vars <- function(...) quos(...)

#' @rdname tidy.recipe
#' @export
tidy.step_impute_bag <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$models),
      model = unname(x$models)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = list(NULL))
  }
  res$id <- x$id
  res
}

#' @export
#' @keywords internal
tidy.step_bagimpute <- tidy.step_impute_bag

# ------------------------------------------------------------------------------

#' @export
tunable.step_impute_bag <- function(x, ...) {
  tibble::tibble(
    name = "trees",
    call_info = list(list(pkg = "dials", fun = "trees", range = c(5L, 25L))),
    source = "recipe",
    component = "step_impute_bag",
    component_id = x$id
  )
}

tunable.step_bagimpute <- tunable.step_impute_bag
