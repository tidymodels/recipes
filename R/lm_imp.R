#' Imputation of numeric variables via a linear model.
#'
#' `step_lmimpute` creates a *specification* of a recipe step that will
#'  create linear regression models to impute missing data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose variables. For
#' `step_lmimpute`, this indicates the variables to be imputed; these variables
#' **must** be of type `numeric`. When used with `imp_vars`, the dots indicates
#' which variables are used to predict the missing data in each variable. See
#' [selections()] for more details. For the `tidy` method, these are not
#' currently used.
#' @param role Not used by this step since no new variables are created.
#' @param impute_with A call to `imp_vars` to specify which variables are used
#'  to impute the variables that can include specific variable names separated
#'  by commas or different selectors (see [selections()]). If a column is
#'  included in both lists to be imputed and to be an imputation predictor, it
#'  will be removed from the latter and not used to impute itself.
#' @param models The [lm()] objects are stored here once the linear models
#'  have been trained by [prep.recipe()].
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  bagged tree object).
#' @keywords datagen
#' @concept preprocessing
#' @concept imputation
#' @export
#' @details For each variable requiring imputation, a linear model is fit
#'  where the outcome is the variable of interest and the predictors are any
#'  other variables listed in the `impute_with` formula. The variable(s) to be
#'  imputed must be of type `numeric`; furthermore, they will be coerced to type
#'  `double` as the missing values will be set as the results of a `predict.lm`.
#'
#'  Note that if a variable that is to be imputed is also in `impute_with`,
#'  this variable will be ignored.
#'
#'  Since this is a regression, the imputation has been set to throw an error if any of
#'  the predictors used for imputing contain missing values.
#'
#' @references Kuhn, M. and Johnson, K. (2013). *Applied Predictive Modeling*.
#'  Springer Verlag.
#' @examples
#' library(modeldata)
#' data("ames")
#'
#' ## assign all 0s in Lot_Frontage to be NA
#' ames_missing <- ames
#' ames_missing[ames$Lot_Frontage == 0, ]$Lot_Frontage <- NA
#'
#' rec <- recipe(head(ames_missing))
#' \dontrun{
#' imputed_ames <- rec %>%
#'   step_lmimpute(Lot_Frontage, impute_with = imp_vars(Lot_Area)) %>%
#'   prep(ames_missing) %>%
#'   bake(ames_missing)
#'
#' sum(is.na(ames_missing[ames$Lot_Frontage == 0, ] ))
#' sum(is.na(imputed_ames[ames$Lot_Frontage == 0, ] ))
#'
#' ## show that the NAs (originally 0) are now imputed linearly
#' par(mfrow=c(2, 1))
#' plot(ames$Lot_Area, ames$Lot_Frontage, col = ifelse(ames$Lot_Frontage == 0, "red", "black"))
#' plot(imputed_ames$Lot_Area, imputed_ames$Lot_Frontage, col = ifelse(is.na(ames_missing$Lot_Frontage), "red", "black"))
#' }

step_lmimpute <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           impute_with = imp_vars(all_predictors()),
           models = NULL,
           skip = FALSE,
           id = rand_id("lmimpute")) {
    if (is.null(impute_with))
      rlang::abort("Please provide some variables to `impute_with`.")

    add_step(
      recipe,
      step_lmimpute_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        impute_with = impute_with,
        models = models,
        skip = skip,
        id = id
      )
    )
  }

step_lmimpute_new <-
  function(terms, role, trained, models, impute_with,
           skip, id) {
    step(
      subclass = "lmimpute",
      terms = terms,
      role = role,
      trained = trained,
      impute_with = impute_with,
      models = models,
      skip = skip,
      id = id
    )
  }


lm_wrap <- function(vars, dat) {
  dat <- as.data.frame(dat[, c(vars$y, vars$x)])

  if (!is.numeric(dat[[vars$y]]))
    rlang::abort(
      glue::glue(
        "Variable '{vars$y}' chosen for linear regression imputation ",
        "must be of type numeric."
    ))

  out <- lm(as.formula(paste0(vars$y, "~", ".")), data = dat)
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
prep.step_lmimpute <- function(x, training, info = NULL, ...) {
  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      info = info
    )

  x$models <- lapply(
    var_lists,
    lm_wrap,
    dat = training
  )
  names(x$models) <- vapply(var_lists, function(x) x$y, c(""))

  step_lmimpute_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    models = x$models,
    impute_with = x$impute_with,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lmimpute <- function(object, new_data, ...) {
  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows))
    return(new_data)

  old_data <- new_data
  for (i in seq(along.with = object$models)) {
    imp_var <- names(object$models)[i]
    missing_rows <- !complete.cases(new_data[, imp_var])
    if (any(missing_rows)) {
      preds <- object$models[[imp_var]]$..imp_vars
      pred_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if (any(is.na(pred_data))) {
        rlang::warn("
          There were missing values in the predictor(s);
          imputation did not occur.
        ")
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


print.step_lmimpute <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Linear regression imputation for ", sep = "")
    printer(names(x$models), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_lmimpute
#' @param x A `step_lmimpute` object.
#' @export
tidy.step_lmimpute <- function(x, ...) {
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
