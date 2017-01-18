#' Create a Recipe for Preprocessing Data
#'
#' A recipe is a description of what steps should be applied to a data set in order to get it ready for data analysis.
#'
#' @aliases recipe recipe.default recipe.formula
#' @param x an object
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
recipe.default <- function(data, vars = names(data), roles = NULL, ...) {

  if(!is_tibble(data)) data <- as_tibble(data)
  if(is.null(vars)) vars <- colnames(data)
  if(any(table(vars) > 1))
    stop("`vars` should have unique members")
  if(any(!(vars %in% colnames(data))))
    stop("1+ elements of `vars` are not in `data`")

  data <- data[, vars]

  var_info <- tibble(variable = vars)

  ## Check and add roles when available
  if(!is.null(roles)) {
    if(length(roles) != length(vars))
      stop("The number of roles should be the same as the number of variables")
    var_info$role <- roles
  } else var_info$role <- ""

  ## Add types
  var_info <- full_join(get_types(data), var_info)
  var_info$source <- "original"

  ## Return final object of class `recipe`
  out <- list(var_info = var_info,
              term_info = var_info,
              steps = NULL,
              template = data)
  class(out) <- "recipe"
  out
}

#' @rdname recipe
#' @param formula a model formula.
#' @export
#' @importFrom stats as.formula
#' @importFrom tibble as_tibble is_tibble 

recipe.formula <- function(formula, data, ...) {
  if(!is_formula(formula))
    formula <- as.formula(formula)

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

  recipe.default(data = data, vars = vars, roles = roles, ...)
}


#' @aliases learn learn.recipe
#' @param x an object
#' @param ... further arguments passed to or from other methods (not currently used).
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @exportPattern ^learn
learn   <- function(x, ...) UseMethod("learn")

#' Train a Data Recipe
#'
#' For a recipe with at least one preprocessing step, estimate the required parameters from a training set that can be later applied to other data sets.
#' @param training A data frame or tibble that will be used to estimate parameters for preprocessing.
#' @param verbose A logical that controls wether progress is reported as steps are executed.
#' @return A recipe whose step objects have been updated with the required quantities (e.g. parameter estimates, model objects, etc). Also, the \code{term_info} object is likely to be modified as the steps are executed.
#' @rdname learn
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom dplyr left_join
learn.recipe <- function(x, training = x$template, verbose = TRUE, ...) {
  if(length(x$steps) == 0)
    stop("Add some steps")

  training <- if(!is_tibble(training))
    as_tibble(training[, x$var_info$variable, drop = FALSE]) else
      training[, x$var_info$variable]

  for(i in seq(along = x$steps)) {
    if(verbose) cat("step", i, "\n")

    # Compute anything needed for the pre-processing steps
    # then apply it to the current training set

    x$steps[[i]] <- learn(x$steps[[i]], data = training)
    training <- process(x$steps[[i]], data = training)
    x$term_info <- left_join(get_types(training), x$term_info)

    ## Update the roles and the term source
    ## These next two steps needs to be smarter to find diffs
    if(!is.na(x$steps[[i]]$role))
      x$term_info$role[is.na(x$term_info$role)] <- x$steps[[i]]$role
    x$term_info$source[is.na(x$term_info$source)] <- "derived"
  }
  x
}

#' @rdname process
#' @aliases process process.recipe
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @exportPattern ^process
process <- function(x, ...) UseMethod("process")

#' Apply a Trained Data Recipe
#'
#' For a recipe with at least one preprocessing step that has been trained by \code{\link{learn.recipe}}, apply the computations to new data.
#' @param x A trained object such as a \code{\link{recipe}} with at least one preprocessing step.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @param newdata A data frame or tibble for whom the preprocessing will be applied.
#' @return A tibble that may have different columns than the original columns in \code{newdata}.
#' @rdname process
#' @importFrom tibble as_tibble is_tibble

process.recipe <- function(x, newdata = x$template, ...) {
  newdata <- if(!is_tibble(newdata))
    as_tibble(newdata[, x$var_info$variable, drop = FALSE]) else
      newdata[, x$var_info$variable]

  for(i in seq(along = x$steps)) {
    newdata <- process(x$steps[[i]], data = newdata)
  }
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
#' @export print.recipe
print.recipe <- function(x, form_width = 30, ...) {
  tab <- as.data.frame(table(x$var_info$role))
  colnames(tab) <- c("role", "#variables")
  cat("Data Recipe\n\n")
  cat("Inputs:\n\n")
  print(tab, row.names = FALSE)


  if(!is.null(x$steps)) {
    cat("\nSteps:\n\n")
    for(i in seq_along(x$steps))
      print(x$steps[[i]], form_width = form_width)
  }
  invisible(x)
}
