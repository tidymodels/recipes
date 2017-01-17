#' Declare Which Variables Should Be Converted to Dummy Variables.
#' 
#' This function is a \emph{specification} of a recipe step that will convert nominal data (e.g. character or factors) into one or more numeric binary model terms for the levels of the original data. For example, if a factor column in the data set has levels of "red", "green", "blue", the dummy variable process will create two additional columns of 0/1 data for two of those three values (and remove the original column).
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used to create the dummy variables.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the binary dummy variable columns created by the original variables will be used as predictors in a model. 
#' @param contrast A specification for which type of contrast should be used to make a set of full rank dummy variables. See \code{\link[stats]{contrasts}} for more details. \bold{not currently hooked up}
#' @param naming A function that defines the naming convention for new binary columns. See Details below. 
#' @param levels A list that contains the information needed to create dummy variables for each variable contained in \code{terms}. This is \code{NULL} until the step is trained by \code{\link{learn.dummy_step}}.
#' @return An object of class \code{dummy_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification dummy_variables variable_encodings
#' @export

step_dummy <- function(recipe, 
                       terms, 
                       role = "predictor", 
                       contrast = options("contrasts"),
                       naming = function(var, lvl) 
                         paste(var, make.names(lvl), sep = "_"),
                       levels = NULL) {
  add_step(
    recipe, 
    step_dummy_new(
      terms = terms, 
      role = role,
      contrast = contrast, 
      naming = naming, 
      levels = levels))
}

step_dummy_new <- function(terms = NULL, 
                           role = "predictor",
                           contrast = contrast, 
                           naming = naming, 
                           levels = levels) {
  step(
    subclass = "dummy", 
    terms = terms,
    role = role,
    contrast = contrast,
    naming = naming,
    levels = levels
  )
}


#' Estimate Dummy Variable Encoding from a Training Set.
#' 
#' For a training set of data, this function enumerates the possible values of the variables so that dummy variables can be created when a specific data set is \emph{processed} (see \code{\link{process.dummy_step}}). 
#' 
#' @param x a \code{dummy_step} object that specifies which columns will be converted to dummy variables.
#' @param data a tibble or data frame that contains the training set. These data will be used to define the dummy variables for all future data when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{dummy_step}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification dummy_variables variable_encodings
#' @export
#' @importFrom stats as.formula model.frame

learn.dummy_step <- function(x, data, ...) {
  col_names <- filter_terms(x$terms, data) 
  
  ## I hate doing this but currently we are going to have 
  ## to save the terms object form the original (= training) 
  ## data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  for(i in seq_along(col_names)) {
    form <- as.formula(paste0("~", col_names[i]))
    terms <- model.frame(
      form, 
      data = data, 
      xlev = x$levels[[i]]
    )
    levels[[i]] <- attr(terms, "terms")
  }
  
  step_dummy_new(
    terms = x$terms,
    role = x$role,
    contrast = x$contrast,
    naming = x$naming,
    levels = levels
  )
}

#' Generate dummy variables in a data set.
#' 
#' For a trained \code{dummy_step} object, this function can be used to apply the process of creating dummy variables to any data set. This creates new columns in the data set and removes the original column(s). 
#' 
#' @param x A trained \code{dummy_step} object.
#' @param data A tibble or data frame that has nominal variables that will be converted to dumy variables.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification dummy_variables variable_encodings
#' @export
#' @importFrom stats as.formula model.matrix
#' @importFrom tibble as_tibble

process.dummy_step <- function(x, data, ...) {
  ## Maybe do this in C? 
  col_names <- names(x$levels)
  for(i in seq_along(x$levels)) {
    form <- as.formula(paste0("~", x$levels[i]))
    indicators <- model.matrix(
      object = x$levels[[i]], 
      data = data
      # contrasts.arg = x$contrast 
    )
    indicators <- indicators[, -1, drop = FALSE]
    ## use backticks for nonstandard factor levels here 
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- x$naming(col_names[i], used_lvl)
    data <- cbind(data, as.data.frame(indicators))
    data[, col_names[i]] <- NULL
  }
  as_tibble(data)
}

print.dummy_step <- function(x, form_width = 30, ...) {
  cat("Dummy variables from ")
  cat(form_printer(x, wdth = form_width))
  if(!is.null(x$levels)) cat(" [learned]\n") else cat("\n")
  invisible(x)
}
