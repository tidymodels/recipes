#' Declare Which Variables Will be Imputed via Bagged Trees.
#' 
#' This function is a \emph{specification} of a recipe step that will create bagged tree models to impute missing data. 
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be imputed.
#' @param role Not used by this step since no new variables are created.  
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param impute_with A representation of the variables that will be used as predictors in the imputation model. If a column is included in both \code{terms} and \code{impute_with}, it will be removed from the latter.  
#' @param options A list of options to \code{\link[ipred]{ipredbagg}}. Defaults are set for the arguments \code{nbagg} and \code{keepX} but others can be passed in. \bold{Note} that the arguments \code{X} and \code{y} should not be passed here.
#' @param seed_val A integer used to create reproducible models. The same seed is used across all imputation models. 
#' @param models The \code{\link[ipred]{ipredbagg}} objects are stored here once this bagged trees have be trained by \code{\link{learn.bagimpute_step}}.
#' @return An object of class \code{bagimpute_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing imputation
#' @export

step_bagimpute <- function(recipe, terms, role = NA,
                           trained = FALSE, 
                           models = NULL, 
                           options = list(nbagg = 25, keepX = FALSE),
                           impute_with = NULL,
                           seed_val = sample.int(10^4, 1)) {
  if(is.null(impute_with))
    stop("Please list some variables in `impute_with`")
  add_step(
    recipe, 
    step_bagimpute_new(
      terms = terms, 
      role = role,
      trained = trained,
      models = models,
      options = options,
      impute_with = impute_with,
      seed_val = seed_val
    )
  )
}

step_bagimpute_new <- function(terms = NULL, role = NA,
                               trained = FALSE, 
                               models = NULL, options = NULL,
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
  mod_form <- as.formula(paste0(vars$y,"~."))
  dat <- as.data.frame(dat[, c(vars$y, vars$x)])
  if(!is.null(seed_val) && !is.na(seed_val))
    set.seed(seed_val) 
  
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

## This figures out which data should be used to predict each variable scheduled for imputation
impute_var_lists <- function(to_impute, impute_using, dat) {
  to_impute <- filter_terms.formula(to_impute, dat) 
  impute_using <- filter_terms.formula(impute_using, dat) 
  var_lists <- vector(mode = "list", length = length(to_impute))
  for(i in seq_along(var_lists)) {
    var_lists[[i]] <- list(
      y = to_impute[i], 
      x = impute_using[!(impute_using %in% to_impute[i])]
    )
  }
  var_lists
}


#' Estimate the Bagged Tree Imputation Models from a Training Set.
#' 
#' For a training set of data, this function uses \code{\link[ipred]{ipredbagg}} to create models that will be used to impute missing data. This transformation only fits the models while \code{\link{process}} is used to do the imputation for specific data sets. 
#'
#' @param x a \code{bagimpute_step} object that contains the model objects.
#' @param data a tibble or data frame that contains the training set. These data will be used to compute the tree ensembles that are used when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{bagimpute_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing imputation
learn.bagimpute_step <- function(x, data, ...) {
  var_lists <- impute_var_lists(x$terms,x $impute_with, data) 
  x$models <- lapply(
    var_lists, 
    bag_wrap,
    dat = data,
    opt = x$options, 
    seed_val = x$seed_val
  )
  names(x$models) <- vapply(var_lists, function(x) x$y, c(""))
  x$trained <- TRUE
  x
}

#' Impute Missing Data a Data Set using Bagged Trees.
#' 
#' For a trained \code{bagimpute_step} object, this function imputes data based on models created on the training set. 
#' 
#' @param x A trained \code{bagimpute_step} object.
#' @param data A tibble or data frame that will be imputed.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing imputation
#' @importFrom tibble as_tibble
#' @importFrom stats predict complete.cases
process.bagimpute_step <- function(x, data, ...) {
  missing_rows <- !complete.cases(data)
  if(!any(missing_rows))
    return(data)
  
  old_data <- data
  for(i in seq(along = x$models)) {
    imp_var <- names(x$models)[i]
    missing_rows <- !complete.cases(data[, imp_var])
    if(any(missing_rows)) {
      preds <- x$models[[i]]$..imp_vars
      pred_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if(all(is.na(pred_data))) {
        warning("All predictors are missing; cannot impute")
      } else {
        pred_vals <- predict(x$models[[i]], pred_data)
        data[missing_rows, imp_var] <- pred_vals
      }
    }
  }
  ## changes character to factor!
  as_tibble(data)
}


#' @export
print.bagimpute_step <- function(x, form_width = 30, ...) {
  cat("Bagged tree imputation for ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}