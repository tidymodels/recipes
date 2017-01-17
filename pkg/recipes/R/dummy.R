#' Declare which variables should be converted to dummy variables.
#' 
#' This function is a \emph{specification} of a recipe step that will convert nominal data (e.g. character or factors) into one or more numeric binary model terms for the levels of the original data. For example, if a factor column in the data set has levels of "red", "green", "blue", the dummy variable process will create two additional columns of 0/1 data for two of those three values (and remove the original column).
#' 
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param terms A representation of the variables or terms that will be used to create the dummy variables.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the binary dummy variable columns created by the original variables will be used as predictors in a model. 
#' @return An object of class \code{step_dummy}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification
#' @export

step_dummy <- function(recipe, terms, role = "predictor") {
  add_step(recipe, step_dummy_new(terms = terms, role = role))
}

## This is the function that instantiates a new dummy variable object. 
## The `naming`` function defines how the novel terms will be named. In this function `var` is the name of the original variable that created the dummy variables and `lvl` is a vector of levels for the factor. 
## The `levels` argument should be a list. Each element in the list contains the data required to produce the dummy variables (such as the possible values). 
step_dummy_new <- function(terms = NULL, 
                           role = "predictor", 
                           contrast = options("contrasts"),
                           naming = function(var, lvl) 
                             paste(var, make.names(lvl), sep = "_"),
                           levels = NULL) {
  step(
    subclass = "dummy", 
    terms = terms,
    role = role,
    contrast = contrast,
    naming = naming,
    levels = levels
  )
}


#' Estimate dummy variable encoding from a training set.
#' 
#' For a training set of data, this function enumerates the possible values of the variables so that dummy variables can be created when a specific data set is \emph{processed} (see \code{\link{process.dummy_step}}). 
#' 
#' @param x a \code{step_dummy} object that specifies which columns will be converted to dummy variables.
#' @param data a tibble or data frame that contains the training set. These data will be used to define the dummy variables for all future data when this step is applied.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return An object of class \code{step_dummy}. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification
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
#' For a trained \code{step_dummy} object, this function can be used to apply the process of creating dummy variables to any data set. This creates new columns in the data set and removes the original column(s). 
#' 
#' @param x A trained \code{step_dummy} object.
#' @param data A tibble or data frame that has nominal variables that will be converted to dumy variables.
#' @param ... further arguments passed to or from other methods (not currently used).
#' @return A tibble of processed data. 
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification
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
