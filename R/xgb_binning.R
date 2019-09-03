#' Discretize numeric variables with XgBoost
#'
#' `step_xgb_binning` creates a *specification* of a recipe
#'  step that will discretize numeric data (e.g. integeres or doubles)
#'  into bins in a supervised way using an XgBoost model.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param outcome A character string to indicate the outcome variable to train XbBoost models.
#' @param rules The splitting rules of the best XgBoost tree to retain for each variable.
#' @param prefix A character string that will be the prefix to the resulting new variables.
#' Defaults to "xgb_bin".
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @keywords binning
#' @concept preprocessing
#' @concept xgboost
#' @concept discretization
#' @concept factors
#' @export
#' @details step_xgb_binning creates non-uniform bins from numerical
#'  variables by utilizing the information about the outcome
#'  variable and applying the xgboost model. It is advised to impute
#'  missing values before this step. This step is intented to be
#'  used particularly with linear models because thanks to creating
#'  non-uniform bins it becomes easier to learn non-linear patterns
#'  from the data.
#'
#' This step requires the \pkg{xgboost} package. If not installed, the
#'  step will stop with a note about installing the package.
#'
#' Note that the original variables will be replaced with the new bins.
#'  The new variables will have names that begin with `prefix` followed
#'  by their original name.
#'
#' @examples
#' data(credit_data)
#' library(rsample)
#'
#' split <- initial_split(credit_data, strata = "Status")
#'
#' credit_data_tr <- training(split)
#' credit_data_te <- testing(split)
#'
#' xgb_rec <- recipe(Status ~ ., data = credit_data_tr) %>%
#'   step_medianimpute(all_numeric()) %>%
#'   step_xgb_binning(all_numeric(), outcome = "Status")
#'
#' xgb_rec <- prep(xgb_rec, training = credit_data_tr, retain = TRUE)
#'
#' xgb_test_bins <- bake(xgb_rec, credit_data_te)
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
#'

step_xgb_binning <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           rules = NULL,
           prefix = "xgb_bin",
           skip = FALSE,
           id = rand_id("xgb_binning")) {
    if (is.null(outcome))
      stop("`outcome` should select at least one column.", call. = FALSE)

    # recipes_pkg_check("xgb_binninb") - TO DO CHECKS

    add_step(
      recipe,
      step_xgb_binning_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        rules = rules,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_xgb_binning_new <-
  function(terms, role, trained, outcome, rules,
           prefix, skip, id) {
    step(
      subclass = "xgb_binning",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      rules = rules,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

run_xgboost <- function(.train, .test, .max_bin){
  xgboost::xgb.train(
    params = list(
      eta = 0.3,
      max_bin = .max_bin,
      max_depth = 1,
      min_child_weight = 5
    ),
    nrounds = 100,
    data = .train,
    watchlist = list(
      train = .train,
      test  = .test
    ),
    tree_method = "hist",
    early_stopping_rounds = 10,
    objective = "binary:logistic",
    eval_metric = "auc",
    verbose = 0
  )
}

xgb_binning <- function(df, target, variable){

  levels <- sort(unique(df[[target]]))

  if(length(levels) != 2){
    stop("Target variable does not have exactly two levels.", call. = FALSE)
  }

  if(any(is.na((levels)))){
    stop("Target variable contains missing values.", call. = FALSE)
  }

  split <- rsample::initial_split(df, prop = 0.8, strata = target)
  train <- rsample::training(split)
  test  <- rsample::testing(split)

  xgb_train <- xgboost::xgb.DMatrix(
    data = as.matrix(train[, variable]),
    label = ifelse(train[[target]] == levels[[1]], 0, 1)
  )

  xgb_test <- xgboost::xgb.DMatrix(
    data = as.matrix(test[, variable]),
    label = ifelse(test[[target]] == levels[[1]], 0, 1)
  )

  xgb_runs <- purrr::map(c(10, 20, 50), function(x) run_xgboost(xgb_train, xgb_test, x))

  xgb_runs_results <- rbind(
    xgb_runs[[1]]$evaluation_log[xgb_runs[[1]]$best_iteration, ],
    xgb_runs[[2]]$evaluation_log[xgb_runs[[2]]$best_iteration, ],
    xgb_runs[[3]]$evaluation_log[xgb_runs[[3]]$best_iteration, ]
  ) %>%
  cbind(model = c("mdl_max_bin_10", "mdl_max_bin_20", "mdl_max_bin_50"))

  xgb_best <- xgb_runs_results$model[which.max(xgb_runs_results$test_auc)]

  xgb_selected <- if (xgb_best == "mdl_max_bin_50") {
    xgb_runs[[3]]
  } else if (xgb_best == "mdl_max_bin_20") {
    xgb_runs[[2]]
  } else {
    xgb_runs[[1]]
  }

  xgboost::xgb.model.dt.tree(
    model = xgb_selected,
    trees = xgb_selected$best_iteration,
    use_int_id = TRUE
  )

}

#' @export
prep.step_xgb_binning <- function(x,
                                  training,
                                  info = NULL,
                                  ...) {

  col_names <- terms_select(terms = x$terms, info = info)
  check_type(training[, col_names])

  rules <- vector("list", length(col_names))

  for(i in seq_along(col_names)){
    if(col_names[[i]] == x$outcome){
      next
    } else {
      rules[[i]] <- xgb_binning(training, x$outcome, col_names[[i]])
    }
  }

  names(rules) <- col_names

  step_xgb_binning_new(
    terms   = x$terms,
    role    = x$role,
    trained = TRUE,
    outcome = x$outcome,
    rules   = rules,
    prefix  = x$prefix,
    skip    = x$skip,
    id      = x$id
  )
}

#' @export
bake.step_xgb_binning <- function(object,
                                  new_data,
                                  ...) {

  vars <- object$rules

  for(i in seq_along(vars)){

    var <- names(vars)[[i]]

    new_data[, var] <- cut(
      new_data[[var]],
      breaks = c(
        -Inf,
        unique(vars[[var]]$Split),
        Inf
      ),
      include.lowest = TRUE,
      right = FALSE,
      dig.lab = 4
    )

    names(new_data)[names(new_data) == var] <- paste0(object$prefix, "_", var)
  }
  as_tibble(new_data)
}

print.step_xgb_binning <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretizing variables using XgBoost ")
    printer(names(x$rules), x$terms, x$trained, width = width)
    invisible(x)
  }

