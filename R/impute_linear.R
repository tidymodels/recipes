#' Impute numeric variables via a linear model
#'
#' `step_impute_linear` creates a *specification* of a recipe step that will
#'  create linear regression models to impute missing data.
#'
#' @inheritParams step_impute_bag
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables to be imputed;
#' these variables **must** be of type `numeric`. When used with `imp_vars`,
#' these dots indicate which variables are used to predict the missing data
#' in each variable. See [selections()] for more details.
#' @param models The [lm()] objects are stored here once the linear models
#'  have been trained by [prep()].
#' @template step-return
#' @family imputation steps
#' @export
#' @details For each variable requiring imputation, a linear model is fit
#'  where the outcome is the variable of interest and the predictors are any
#'  other variables listed in the `impute_with` formula. Note that if a variable
#'  that is to be imputed is also in `impute_with`, this variable will be ignored.
#'
#' The variable(s) to be imputed must be of type `numeric`. The imputed values
#'  will keep the same type as their original data (i.e, model predictions are
#'  coerced to integer as needed).
#'
#'  Since this is a linear regression, the imputation model only uses complete
#'  cases for the training set predictors.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with
#'  columns `terms` (the selectors or variables selected) and `model` (the
#'  bagged tree object) is returned.
#'
#' @template case-weights-unsupervised
#'
#' @references Kuhn, M. and Johnson, K. (2013).
#' *Feature Engineering and Selection*
#' \url{https://bookdown.org/max/FES/handling-missing-data.html}
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2"))
#' data(ames, package = "modeldata")
#' set.seed(393)
#' ames_missing <- ames
#' ames_missing$Longitude[sample(1:nrow(ames), 200)] <- NA
#'
#' imputed_ames <-
#'   recipe(Sale_Price ~ ., data = ames_missing) %>%
#'   step_impute_linear(
#'     Longitude,
#'     impute_with = imp_vars(Latitude, Neighborhood, MS_Zoning, Alley)
#'   ) %>%
#'   prep(ames_missing)
#'
#' imputed <-
#'   bake(imputed_ames, new_data = ames_missing) %>%
#'   dplyr::rename(imputed = Longitude) %>%
#'   bind_cols(ames %>% dplyr::select(original = Longitude)) %>%
#'   bind_cols(ames_missing %>% dplyr::select(Longitude)) %>%
#'   dplyr::filter(is.na(Longitude))
#'
#' library(ggplot2)
#' ggplot(imputed, aes(x = original, y = imputed)) +
#'   geom_abline(col = "green") +
#'   geom_point(alpha = .3) +
#'   coord_equal() +
#'   labs(title = "Imputed Values")
step_impute_linear <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           impute_with = imp_vars(all_predictors()),
           models = NULL,
           skip = FALSE,
           id = rand_id("impute_linear")) {
    if (is.null(impute_with)) {
      rlang::abort("Please provide some variables to `impute_with`.")
    }

    add_step(
      recipe,
      step_impute_linear_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        impute_with = impute_with,
        models = models,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_impute_linear_new <-
  function(terms, role, trained, models, impute_with,
           skip, id, case_weights) {
    step(
      subclass = "impute_linear",
      terms = terms,
      role = role,
      trained = trained,
      impute_with = impute_with,
      models = models,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

lm_wrap <- function(vars, dat, wts = NULL, call = caller_env(2)) {
  dat <- as.data.frame(dat[, c(vars$y, vars$x)])
  complete <- stats::complete.cases(dat)
  dat <- dat[complete, ]
  wts <- wts[complete]
  if (nrow(dat) == 0) {
    rlang::abort(
      paste(
        "The data used by step_impute_linear() did not have any rows",
        "where the imputation values were all complete."
      ),
      call = call
    )
  }

  if (!is.numeric(dat[[vars$y]])) {
    rlang::abort(
      glue(
        "Variable '{vars$y}' chosen for linear regression imputation ",
        "must be of type numeric."
      ),
      call = call
    )
  }

  if (is.null(wts)) {
    wts <- rep(1, nrow(dat))
  } else {
    wts <- as.double(wts)
  }

  out <- lm(as.formula(paste0(vars$y, "~", ".")), data = dat, weights = wts,
            model = FALSE)
  out$..imp_vars <- vars$x
  attr(out$terms, ".Environment") <- rlang::base_env()

  ## remove other unneeded elements for predict
  out$call <- NULL
  out$assign <- NULL
  out$fitted.values <- NULL
  out$df.residual <- NULL
  out$residuals <- NULL
  out$qr$qr <- NULL
  out$effects <- NULL

  out
}

#' @export
prep.step_impute_linear <- function(x, training, info = NULL, ...) {

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  var_lists <-
    impute_var_lists(
      to_impute = x$terms,
      impute_using = x$impute_with,
      training = training,
      info = info
    )

  x$models <- lapply(
    var_lists,
    lm_wrap,
    dat = training,
    wts = wts
  )

  names(x$models) <- vapply(var_lists, function(x) x$y, c(""))

  step_impute_linear_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    models = x$models,
    impute_with = x$impute_with,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_impute_linear <- function(object, new_data, ...) {
  check_new_data(names(object$models), object, new_data)

  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }

  old_data <- new_data
  for (i in seq(along.with = object$models)) {
    imp_var <- names(object$models)[i]
    missing_rows <- !complete.cases(new_data[[imp_var]])
    if (any(missing_rows)) {
      preds <- object$models[[imp_var]]$..imp_vars
      pred_data <- old_data[missing_rows, preds, drop = FALSE]
      ## do a better job of checking this:
      if (any(is.na(pred_data))) {
        rlang::warn("
          There were missing values in the predictor(s) used to impute;
          imputation did not occur.
        ")
      } else {
        pred_vals <- predict(object$models[[imp_var]], pred_data)
        pred_vals <- cast(pred_vals, new_data[[imp_var]])
        new_data[[imp_var]] <- vec_cast(new_data[[imp_var]], pred_vals)
        new_data[missing_rows, imp_var] <- pred_vals
      }
    }
  }
  new_data
}

#' @export
print.step_impute_linear <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Linear regression imputation for "
    print_step(names(x$models), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_impute_linear <- function(x, ...) {
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
