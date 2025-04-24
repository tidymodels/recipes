#' Impute via bagged trees
#'
#' `step_impute_bag()` creates a *specification* of a recipe step that will
#' create bagged tree models to impute missing data.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables to be imputed.
#'   When used with `imp_vars`, these dots indicate which variables are used to
#'   predict the missing data in each variable. See [selections()] for more
#'   details.
#' @param impute_with Bare names or selectors functions that specify which
#'   variables are used to impute the variables that can include specific
#'   variable names separated by commas or different selectors (see
#'   [selections()]). If a column is included in both lists to be imputed and to
#'   be an imputation predictor, it will be removed from the latter and not used
#'   to impute itself.
#' @param trees An integer for the number of bagged trees to use in each model.
#' @param options A list of options to [ipred::ipredbagg()]. Defaults are set
#'   for the arguments `nbagg` and `keepX` but others can be passed in. **Note**
#'   that the arguments `X` and `y` should not be passed here.
#' @param seed_val An integer used to create reproducible models. The same seed
#'   is used across all imputation models.
#' @param models The [ipred::ipredbagg()] objects are stored here once this
#'   bagged trees have be trained by [prep()].
#' @template step-return
#' @family imputation steps
#' @export
#' @details
#'
#' For each variable requiring imputation, a bagged tree is created where the
#' outcome is the variable of interest and the predictors are any other
#' variables listed in the `impute_with` formula. One advantage to the bagged
#' tree is that is can accept predictors that have missing values themselves.
#' This imputation method can be used when the variable of interest (and
#' predictors) are numeric or categorical. Imputed categorical variables will
#' remain categorical. Also, integers will be imputed to integer too.
#'
#' Note that if a variable that is to be imputed is also in `impute_with`, this
#' variable will be ignored.
#'
#' It is possible that missing values will still occur after imputation if a
#' large majority (or all) of the imputing variables are also missing.
#'
#' As of `recipes` 0.1.16, this function name changed from `step_bagimpute()` to
#' `step_impute_bag()`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `model` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{model}{list, the bagged tree object}
#'   \item{id}{character, id of this step}
#' }
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
#' impute_rec <- rec |>
#'   step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te)
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#'
#' ## Specifying which variables to imputate with
#'
#' impute_rec <- rec |>
#'   step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt,
#'     impute_with = c(Time, Age, Expenses),
#'     # for quick execution, nbagg lowered
#'     options = list(nbagg = 5, keepX = FALSE)
#'   )
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, new_data = credit_te)
#'
#' credit_te[missing_examples, ]
#' imputed_te[missing_examples, names(credit_te)]
#'
#' tidy(impute_rec, number = 1)
#' tidy(imp_models, number = 1)
#' }
step_impute_bag <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    impute_with = all_predictors(),
    trees = 25,
    models = NULL,
    options = list(keepX = FALSE),
    seed_val = sample.int(10^4, 1),
    skip = FALSE,
    id = rand_id("impute_bag")
  ) {
    add_step(
      recipe,
      step_impute_bag_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        impute_with = enquos(impute_with),
        trees = trees,
        models = models,
        options = options,
        seed_val = seed_val,
        skip = skip,
        id = id
      )
    )
  }

step_impute_bag_new <-
  function(
    terms,
    role,
    trained,
    models,
    options,
    impute_with,
    trees,
    seed_val,
    skip,
    id
  ) {
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
  out <- rlang::try_fetch(
    do.call(
      "ipredbagg",
      c(
        list(
          y = dat[, vars$y],
          X = dat[, vars$x, drop = FALSE]
        ),
        opt
      )
    ),
    error = function(cnd) {
      if (grepl("number of rows of matrices must match", cnd$message)) {
        cli::cli_abort(
          c(
            x = "The bagged tree model was not able to fit to {.col {vars$y}}.
          It appears to be because it had near zero variance.",
            i = "Please deselect it for this step."
          ),
          call = call("prep")
        )
      }

      cli::cli_abort("Failed to compute:", parent = cnd, call = call("prep"))
    }
  )
  out$..imp_vars <- vars$x
  out <- butcher_bag_trees(out)
  out
}

butcher_bag_tree <- function(x) {
  x$btree$call <- call("dummy_call")
  attr(x$btree$terms, ".Environment") <- rlang::base_env()
  x$btree$y <- integer()
  x
}

butcher_bag_trees <- function(x) {
  x$mtrees <- map(x$mtrees, butcher_bag_tree)
  x
}

## This figures out which data should be used to predict each variable
## scheduled for imputation
impute_var_lists <- function(
  to_impute,
  impute_using,
  training,
  info,
  call = caller_env()
) {
  to_impute <- recipes_eval_select(to_impute, training, info)
  impute_using <- recipes_argument_select(
    impute_using,
    training,
    info,
    single = FALSE,
    arg_name = "impute_with",
    call = call
  )

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
  check_number_whole(x$trees, arg = "trees", min = 1)
  check_number_whole(x$seed_val, arg = "seed_val")
  check_options(x$options, exclude = c("X", "y"))

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
bake.step_impute_bag <- function(object, new_data, ...) {
  col_names <- names(object$models)
  check_new_data(col_names, object, new_data)

  missing_rows <- !vec_detect_complete(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }

  old_data <- new_data
  for (col_name in col_names) {
    missing_rows <- !vec_detect_complete(new_data[[col_name]])
    if (!any(missing_rows)) {
      next
    }
    preds <- object$models[[col_name]]$..imp_vars
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

    pred_vals <- predict(object$models[[col_name]], imp_data)

    # For an ipred bug reported on 2021-09-14:
    pred_vals <- cast(pred_vals, object$models[[col_name]]$y)
    new_data[missing_rows, col_name] <- pred_vals
  }
  new_data
}

#' @export
print.step_impute_bag <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Bagged tree imputation for "
    print_step(names(x$models), x$terms, x$trained, title, width)
    invisible(x)
  }

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
