#' Discretize numeric variables with XgBoost
#'
#' `step_discretize_tree` creates a *specification* of a recipe
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
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome to train XbBoost models in order to discretize explanatory variables.
#' @param learn_rate The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' Corresponds to 'eta' in the \pkg{xgboost} package. Defaults to 0.3.
#' @param num_breaks The maximum number of discrete bins to bucket continuous features.
#' Corresponds to 'max_bin' in the \pkg{xgboost} package. Defaults to 10.
#' @param tree_depth The maximum depth of the tree (i.e. number of splits).
#' Corresponds to 'max_depth' in the \pkg{xgboost} package. Defaults to 1.
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
#' @details step_discretize_tree creates non-uniform bins from numerical
#'  variables by utilizing the information about the outcome
#'  variable and applying the xgboost model. It is advised to impute
#'  missing values before this step. This step is intented to be
#'  used particularly with linear models because thanks to creating
#'  non-uniform bins it becomes easier to learn non-linear patterns
#'  from the data.
#'
#'  The best selection of buckets for each variable is selected using
#'  an internal early stopping scheme implemented in the \pkg{xgboost}
#'  package, which makes this discretization method prone to overfitting.
#'
#' The pre-defined values of the underlying xgboost learns should give
#' good and reasonably complex results. However, if one wishes to tune them
#' the recommended path would be to first start with changing the value
#' of 'num_breaks' to e.g.: 20 or 30. If that doesn't give satisfactory results
#' one could experiment with increasing the 'tree_depth' parameter.
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
#'   step_discretize_tree(all_numeric(), outcome = "Status")
#'
#' xgb_rec <- prep(xgb_rec, training = credit_data_tr, retain = TRUE)
#'
#' xgb_test_bins <- bake(xgb_rec, credit_data_te)
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
#'

step_discretize_tree <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           learn_rate = 0.3,
           num_breaks = 10,
           tree_depth = 1,
           rules = NULL,
           prefix = "bin_tree",
           skip = FALSE,
           id = rand_id("discretize_tree")) {
    if (is.null(outcome))
      stop("`outcome` should select at least one column.", call. = FALSE)

    # recipes_pkg_check("xgb_binninb") - TO DO CHECKS

    add_step(
      recipe,
      step_discretize_tree_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        learn_rate = learn_rate,
        num_breaks = num_breaks,
        tree_depth = tree_depth,
        rules = rules,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_discretize_tree_new <-
  function(terms, role, trained, outcome, learn_rate, num_breaks,
           tree_depth, rules, prefix, skip, id) {
    step(
      subclass = "discretize_tree",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      learn_rate = learn_rate,
      num_breaks = num_breaks,
      tree_depth = tree_depth,
      rules = rules,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

run_xgboost <- function(.train, .test, .learn_rate, .num_breaks, .tree_depth, .objective){
  xgboost::xgb.train(
    params = list(
      eta = .learn_rate,
      max_bin = .num_breaks,
      max_depth = .tree_depth,
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
    objective = .objective,
    verbose = 0,
    nthread = 1
  )
}

xgb_binning <- function(df, outcome, predictor, learn_rate, num_breaks, tree_depth){

  ### LOCAL TESTING ###
  # data(credit_data)
  # library(rsample)
  #
  # split <- initial_split(credit_data, strata = "Status")
  #
  # credit_data_tr <- training(split)
  # credit_data_te <- testing(split)
  #
  # df <- credit_data_tr
  # outcome <- "Status"
  # predictor <- "Seniority"
  # num_breaks <- 20
  ###

  # Checking the number of levels of the outcome
  levels <- sort(unique(df[[outcome]]))

  if(any(is.na((levels)))){
    stop("Outcome variable contains missing values.", call. = FALSE)
  }

  # Defining the objective function
  objective <- if (length(levels) == 2){
    "binary:logistic"
  } else if (class(df[[outcome]]) %in% c("double", "integer")){
    "reg:squarederror"
  } else {
    stop("Outcome variable needs to have two levels (binary classification)
         or be a double/ integer (regression)", call. = FALSE)
  }

  split <- rsample::initial_split(df, prop = 0.8, strata = outcome)
  train <- rsample::training(split)
  test  <- rsample::testing(split)

  xgb_train <- xgboost::xgb.DMatrix(
    data = as.matrix(train[, predictor]),
    label = ifelse(train[[outcome]] == levels[[1]], 0, 1)
  )

  xgb_test <- xgboost::xgb.DMatrix(
    data = as.matrix(test[, predictor]),
    label = ifelse(test[[outcome]] == levels[[1]], 0, 1)
  )

  xgb_mdl <- withr:::with_seed(
    sample.int(10^6, 1),
    run_xgboost(xgb_train, xgb_test, learn_rate, num_breaks, tree_depth, objective)
    )

  xgb_split <- xgboost::xgb.model.dt.tree(
    model = xgb_mdl,
    trees = xgb_mdl$best_iteration,
    use_int_id = TRUE
    ) %>%
    as_tibble() %>%
    select(Node, Feature, Split, Yes, No, Missing)

}

#' @export
prep.step_discretize_tree <- function(x,
                                  training,
                                  info = NULL,
                                  ...) {

  col_names <- terms_select(terms = x$terms, info = info)
  check_type(training[, col_names])

  rules <- vector("list", length(col_names))

  y_name <- terms_select(terms = x$outcome, info = info)

  for(i in seq_along(col_names)){

    if(col_names[[i]] == y_name){
      next
    } else {
      rules[[i]] <- xgb_binning(training, y_name, col_names[[i]], x$learn_rate, x$num_breaks, x$tree_depth)
    }
  }

  names(rules) <- col_names

  step_discretize_tree_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    learn_rate = x$learn_rate,
    num_breaks = x$num_breaks,
    tree_depth = x$tree_depth,
    rules = rules,
    prefix  = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discretize_tree <- function(object,
                                  new_data,
                                  ...) {

  vars <- object$rules

  for(i in seq_along(vars)){

    var <- names(vars)[[i]]
    binned_data <- new_data

    binned_data[, var] <- cut(
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

    names(binned_data)[names(binned_data) == var] <- paste0(object$prefix, "_", var)
    check_name(binned_data, new_data, object)
    new_data <- binned_data
  }
  as_tibble(new_data)
}

print.step_discretize_tree <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretizing variables using XgBoost ")
    printer(names(x$rules), x$terms, x$trained, width = width)
    invisible(x)
  }
