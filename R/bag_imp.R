#' Imputation via Bagged Trees
#'
#' \code{step_bagimpute} creates a \emph{specification} of a recipe step that 
#'   will create bagged tree models to impute missing data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose variables. For 
#'   \code{step_bagimpute}, this indicates the variables to be imputed. When 
#'   used with \code{imp_vars}, the dots indicates which variables are used to 
#'   predict the missing data in each variable. See \code{\link{selections}} 
#'   for more details.
#' @param role Not used by this step since no new variables are created.
#' @param impute_with A call to \code{imp_vars} to specify which variables are 
#'   used to impute the variables that can inlcude specific variable names 
#'   seperated by commas or different selectors (see 
#'   \code{\link{selections}}).  If a column is included in both lists to be 
#'   imputed and to be an imputation predictor, it will be removed from the 
#'   latter and not used to impute itself.
#' @param options A list of options to \code{\link[ipred]{ipredbagg}}. Defaults 
#'   are set for the arguments \code{nbagg} and \code{keepX} but others can be 
#'   passed in. \bold{Note} that the arguments \code{X} and \code{y} should not 
#'   be passed here.
#' @param seed_val A integer used to create reproducible models. The same seed 
#'   is used across all imputation models.
#' @param models The \code{\link[ipred]{ipredbagg}} objects are stored here 
#'   once this bagged trees have be trained by \code{\link{prep.recipe}}.
#' @keywords datagen
#' @concept preprocessing imputation
#' @export
#' @details For each variables requiring imputation, a bagged tree is created 
#'   where the outcome is the variable of interest and the predictors are any 
#'   other variables listed in the \code{impute_with} formula. One advantage to 
#'   the bagged tree is that is can accept predictors that have missing values 
#'   themselves. This imputation method can be used when the variable of 
#'   interest (and predictors) are numeric or categorical. Imputed categorical 
#'   variables will remain categorical.
#'
#' Note that if a variable that is to be imputed is also in \code{impute_with}, 
#'   this variable will be ignored.
#'
#' It is possible that missing values will still occur after imputation if a 
#'   large majority (or all) of the imputing variables are also missing.
#' @references Kuhn, M. and Johnson, K. (2013). 
#'   \emph{Applied Predictive Modeling}. Springer Verlag.
#' @examples
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
#'
#' impute_rec <- rec %>%
#'   step_bagimpute(Status, Home, Marital, Job, Income, Assets, Debt)
#'
#' imp_models <- prep(impute_rec, training = credit_tr)
#'
#' imputed_te <- bake(imp_models, newdata = credit_te, everything())
#'
#' credit_te[missing_examples,]
#' imputed_te[missing_examples, names(credit_te)]


step_bagimpute <- 
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           models = NULL,
           options = list(nbagg = 25, keepX = FALSE),
           impute_with = imp_vars(all_predictors()),
           seed_val = sample.int(10 ^ 4, 1)) {
    if (is.null(impute_with))
      stop("Please list some variables in `impute_with`", call. = FALSE)
    add_step(
      recipe,
      step_bagimpute_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        models = models,
        options = options,
        impute_with = impute_with,
        seed_val = seed_val
      )
    )
}

step_bagimpute_new <- 
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           models = NULL,
           options = NULL,
           impute_with = NULL,
           seed_val = NA) {
  step(
    subclass = "bagimpute",
    terms = terms,
    role = role,
    trained = trained,
    models = models,
    options = options,
    impute_with = impute_with,
    seed_val = seed_val
  )
}


#' @importFrom ipred ipredbagg
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
  x$models <- lapply(
    var_lists,
    bag_wrap,
    dat = training,
    opt = x$options,
    seed_val = x$seed_val
  )
  names(x$models) <- vapply(var_lists, function(x)
    x$y, c(""))
  x$trained <- TRUE
  x
}

#' @importFrom tibble as_tibble
#' @importFrom stats predict complete.cases
#' @export
bake.step_bagimpute <- function(object, newdata, ...) {
  missing_rows <- !complete.cases(newdata)
  if (!any(missing_rows))
    return(newdata)
  
  old_data <- newdata
  for (i in seq(along = object$models)) {
    imp_var <- names(object$models)[i]
    missing_rows <- !complete.cases(newdata[, imp_var])
    if (any(missing_rows)) {
      preds <- object$models[[i]]$..imp_vars
      pred_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if (all(is.na(pred_data))) {
        warning("All predictors are missing; cannot impute", call. = FALSE)
      } else {
        pred_vals <- predict(object$models[[i]], pred_data)
        newdata[missing_rows, imp_var] <- pred_vals
      }
    }
  }
  ## changes character to factor!
  as_tibble(newdata)
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
