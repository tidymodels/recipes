#' Create a Recipe for Preprocessing Data
#'
#' A recipe is a description of what steps should be applied to a data set in order to get it ready for data analysis.
#'
#' @aliases recipe recipe.default recipe.formula
#' @param x an object. For the default method, \code{x} is a data frame or tibble of the \emph{template} data set (see below).
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
recipe <- function(x, ...) UseMethod("recipe")

#' @rdname recipe
#' @param data a data frame or tibble of the \emph{template} data set (see below).
#' @param vars a character string of column names corresponding to variables that will be used in any context (see below)
#' @param roles a character string (the same length of \code{vars}) that describes a single role that the variable will take. This value could be anything but common roles are \code{"outcome"}, \code{"predictor"}, \code{"case_weight"}, or \code{"ID"}
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{recipe} with sub-objects: \item{var_info}{A tibble containing information about the original data set columns}\item{term_info}{A tibble that contains the current set of terms in the data set. This initially defaults to the same data contained in \code{var_info}.}\item{steps}{A list of \code{step} objects that define the sequence of preprocessing steps that will be applied to data. The default value is \code{NULL}}\item{template}{A tibble of the data. This is initialized to be the same as the data given in the \code{data} argument but can be different after the recipe is trained.}
#'
#' @export
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom dplyr full_join
#' @importFrom stats predict
#' @examples
#'
#' # simple example:
#' data(biomass)
#'
#' # split data
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' # When only predictors and outcomes, a simplified formula can be used.
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' library(magrittr)
#' sp_signed <- rec %>%
#'   step_center(~ is_predictor()) %>%
#'   step_scale(~ is_predictor()) %>%
#'   step_spatialsign(~ is_predictor())
#' sp_signed
#'
#' # now estimate required parameters
#' sp_signed_trained <- learn(sp_signed, training = biomass_tr)
#' sp_signed_trained
#'
#' # apply the preprocessing to a data set
#' test_set_values <- process(sp_signed_trained, newdata = biomass_te)
#'
#' # multivariate example
#'
#' # no need for `cbind(carbon, hydrogen)` for right-hand side
#' multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur, data = biomass)
#' multi_y <- multi_y %>%
#'   step_center(~ is_outcome()) %>%
#'   step_scale(~ is_predictor())
#'
#' multi_y_trained <- learn(multi_y, training = biomass_tr)
#'
#' results <- process(multi_y_trained, biomass_te)

recipe.default <- function(x, vars = colnames(x), roles = NULL, ...) {

  if(!is_tibble(x)) x <- as_tibble(x)
  if(is.null(vars)) vars <- colnames(x)
  if(any(table(vars) > 1))
    stop("`vars` should have unique members", call. = FALSE)
  if(any(!(vars %in% colnames(x))))
    stop("1+ elements of `vars` are not in `x`", call. = FALSE)

  x <- x[, vars]

  var_info <- tibble(variable = vars)

  ## Check and add roles when available
  if(!is.null(roles)) {
    if(length(roles) != length(vars))
      stop("The number of roles should be the same as the number of variables",
           call. = FALSE)
    var_info$role <- roles
  } else var_info$role <- NA

  ## Add types
  var_info <- full_join(get_types(x), var_info, by = "variable")
  var_info$source <- "original"

  ## Return final object of class `recipe`
  out <- list(var_info = var_info,
              term_info = var_info,
              steps = NULL,
              template = x,
              levels = NULL)
  class(out) <- "recipe"
  out
}

#' @rdname recipe
#' @param formula A model formula. No in-line functions should be used here (e.g. \code{log(x)}, \code{x:y}, etc.). These types of transformations should be enacted using \code{step} functions in this package. Dots are allowed as are simple multivariate outcome terms (i.e. no need for \code{cbind}; see Examples).
#' @export
#' @importFrom stats as.formula
#' @importFrom tibble as_tibble is_tibble

recipe.formula <- function(formula, data, ...) {
  if(!is_formula(formula))
    formula <- as.formula(formula)
  ## check for in-line formulas
  check_elements(formula, allowed = NULL)

  if(!is_tibble(data)) data <- as_tibble(data)

  ## use lazyeval to get both sides of the formula
  outcomes <- get_lhs_vars(formula, data)
  predictors <- get_rhs_vars(formula, data)

  ## get `vars` from lhs and rhs of formula

  vars <- c(predictors, outcomes)

  ## subset data columns
  data <- data[, vars]

  ## derive roles
  roles <- rep("predictor", length(predictors))
  if(length(outcomes) > 0)
    roles <- c(roles, rep("outcome", length(outcomes)))

  ## pass to recipe.default with vars and roles

  recipe.default(x = data, vars = vars, roles = roles, ...)
}


#' @aliases learn learn.recipe
#' @param x an object
#' @param ... further arguments passed to or from other methods (not currently used).
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
learn   <- function(x, ...) UseMethod("learn")

#' Train a Data Recipe
#'
#' For a recipe with at least one preprocessing step, estimate the required parameters from a training set that can be later applied to other data sets.
#' @param training A data frame or tibble that will be used to estimate parameters for preprocessing.
#' @param fresh A logical indicating whether already trained steps should be re-trained. If \code{TRUE}, you should pass in a data set to the argument \code{training}.
#' @param verbose A logical that controls wether progress is reported as steps are executed.
#' @param retain A logical: should the \emph{processed} training set be saved into the \code{template} slot of the recipe after training? This is a good idea if you want to add more steps later but want to avoid re-training the existing steps.
#' @param stringsAsFactors A logical: should character columns be converted to factors? This affects the processed training set (when \code{retain = TRUE}) as well as the results of \code{process.recipe}.
#' @return A recipe whose step objects have been updated with the required quantities (e.g. parameter estimates, model objects, etc). Also, the \code{term_info} object is likely to be modified as the steps are executed.
#' @rdname learn
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom dplyr left_join
#' @export
learn.recipe <- function(x, training = NULL, fresh = FALSE, verbose = TRUE,
                         retain = FALSE, stringsAsFactors = TRUE, ...) {
  if(length(x$steps) == 0)
    stop("Add some steps", call. = FALSE)
  if(is.null(training)) {
    if(fresh)
      stop("A training set must be supplied to the `training` argument when `fresh = TRUE`",
           call. = FALSE)
    training <- x$template
  } else {
    training <- if(!is_tibble(training))
      as_tibble(training[, x$var_info$variable, drop = FALSE]) else
        training[, x$var_info$variable]
  }
  if(stringsAsFactors) {
    lvls <- lapply(training, get_levels)
    training <- strings2factors(training, lvls)
  } else lvls <- NULL

  for(i in seq(along = x$steps)) {
    note <- paste("step", i, gsub("^step_", "", class(x$steps[[i]])[1]))
    if(!x$steps[[i]]$trained | fresh) {
      if(verbose)
        cat(note, "training", "\n")

      # Compute anything needed for the pre-processing steps
      # then apply it to the current training set

      x$steps[[i]] <- learn(x$steps[[i]], training = training, info = x$term_info)
      training <- process(x$steps[[i]], newdata = training)
      x$term_info <- left_join(get_types(training), x$term_info, by = c("variable", "type"))

      ## Update the roles and the term source
      ## These next two steps needs to be smarter to find diffs
      if(!is.na(x$steps[[i]]$role))
        x$term_info$role[is.na(x$term_info$role)] <- x$steps[[i]]$role
      x$term_info$source[is.na(x$term_info$source)] <- "derived"
    } else {
      if(verbose)
        cat(note, "[pre-trained]\n")
    }
  }

  ## The steps may have changed the data so reassess the levels
  if(stringsAsFactors)
    lvls <- lapply(training, get_levels)

  if(retain)
    x$template <- training

  x$levels <- lvls
  x
}

#' @rdname process
#' @aliases process process.recipe
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
process <- function(object, ...) UseMethod("process")

#' Apply a Trained Data Recipe
#'
#' For a recipe with at least one preprocessing step that has been trained by \code{\link{learn.recipe}}, apply the computations to new data.
#' @param object A trained object such as a \code{\link{recipe}} with at least one preprocessing step.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @param newdata A data frame or tibble for whom the preprocessing will be applied.
#' @param roles A character vector to choose which types of columns to return (e.g. "predictor"). By default all columns are returned.
#' @return A tibble that may have different columns than the original columns in \code{newdata}.
#' @rdname process
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @export

process.recipe <- function(object, newdata = object$template, roles = "all", ...) {
  newdata <- if(!is_tibble(newdata))
    as_tibble(newdata[, object$var_info$variable, drop = FALSE]) else
      newdata[, object$var_info$variable]

  for(i in seq(along = object$steps)) {
    newdata <- process(object$steps[[i]], newdata = newdata)
    if(!is_tibble(newdata)) as_tibble(newdata)
  }
  if(all(roles != "all")) {
    role <- NULL
    dat_info <- filter(object$term_info, role %in% roles)
    if(nrow(dat_info) == 0) {
      msg <- paste("No matching `roles` were found; returning everything instead",
                   "Existing roles are:",
                   paste0(sort(unique(object$term_info$role)), collapse = ", "))
      warning(msg, call. = FALSE)
    }
    keepers <- dat_info$variable
    newdata <- newdata[, names(newdata) %in% keepers]
  }

  if(!is.null(object$levels))
    newdata <- strings2factors(newdata, object$levels)

  newdata
}

#' Print a Recipe
#'
#' @aliases print.recipe
#' @param x A \code{recipe} object
#' @param form_width The number of characters used to print the variables or terms in a formula
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return The original object (invisibly)
#'
#' @author Max Kuhn
#' @export
print.recipe <- function(x, form_width = 30, ...) {
  cat("Data Recipe\n\n")
  cat("Inputs:\n\n")
  no_role <- is.na(x$var_info$role)
  if(any(!no_role)) {
    tab <- as.data.frame(table(x$var_info$role))
    colnames(tab) <- c("role", "#variables")
    print(tab, row.names = FALSE)
    if(any(no_role)) {
      cat("\n ", sum(no_role), "variables without declared roles\n")
    }
  } else {
    cat(" ", nrow(x$var_info), "variables (no declared roles)\n")
  }

  if(!is.null(x$steps)) {
    cat("\nSteps:\n\n")
    for(i in seq_along(x$steps))
      print(x$steps[[i]], form_width = form_width)
  }
  invisible(x)
}

#' Summarize a Recipe
#'
#' This function prints the current set of variables/features and some of their characteristics.
#' @aliases summary.recipe
#' @param object A \code{recipe} object
#' @param original A logical: show the current set of variables or the original set when the recipe was defined.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble with columns \code{variable}, \code{type}, \code{role}, and \code{source}.
#' @details Note that, until the recipe has been learned, the currrent and original variables are the same.
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' summary(rec)
#' rec <- step_pca(rec, terms = ~ is_numeric(), num = 3)
#' summary(rec) # still the same since not yet learned
#' rec <- learn(rec, training = USArrests)
#' summary(rec)
#' @export
#' @seealso \code{\link{recipe}} \code{\link{parse_terms_formula}} \code{\link{learn.recipe}}
summary.recipe <- function(object, original = FALSE, ...) {
if(original)
  object$var_info else
    object$term_info
}


