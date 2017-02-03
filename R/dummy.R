#' Dummy Variables Creation.
#' 
#' \code{step_dummy} creates a a \emph{specification} of a recipe step that will convert nominal data (e.g. character or factors) into one or more numeric binary model terms for the levels of the original data. For example, if a factor column in the data set has levels of "red", "green", "blue", the dummy variable process will create two additional columns of 0/1 data for two of those three values (and remove the original column).
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to create the dummy variables.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the binary dummy variable columns created by the original variables will be used as predictors in a model. 
#' @param contrast A specification for which type of contrast should be used to make a set of full rank dummy variables. See \code{\link[stats]{contrasts}} for more details. \bold{not currently hooked up}
#' @param naming A function that defines the naming convention for new binary columns. See Details below. 
#' @param levels A list that contains the information needed to create dummy variables for each variable contained in \code{terms}. This is \code{NULL} until the step is trained by \code{\link{learn.step_dummy}}.
#' @return \code{step_dummy} and \code{learn.step_dummy} return objects of class \code{step_dummy}.
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification dummy_variables variable_encodings
#' @export

step_dummy <- function(recipe, 
                       terms, 
                       role = "predictor",
                       trained = FALSE, 
                       contrast = options("contrasts"),
                       naming = function(var, lvl) 
                         paste(var, make.names(lvl), sep = "_"),
                       levels = NULL) {
  add_step(
    recipe, 
    step_dummy_new(
      terms = terms, 
      role = role,
      trained = trained,
      contrast = contrast, 
      naming = naming, 
      levels = levels))
}

step_dummy_new <- function(terms = NULL, 
                           role = "predictor",
                           trained = FALSE,
                           contrast = contrast, 
                           naming = naming, 
                           levels = levels) {
  step(
    subclass = "dummy", 
    terms = terms,
    role = role,
    trained = trained,
    contrast = contrast,
    naming = naming,
    levels = levels
  )
}


#' For a training set of data, \code{learn.step_dummy} enumerates the possible values of the variables so that dummy variables can be created when a specific data set is \emph{processed}. 
#' 
#' @param x a \code{step_dummy} object that specifies which columns will be converted to dummy variables.
#' @param training A tibble or data frame that contains the training set. These data will be used to define the dummy variables for all future data when this step is applied.
#' @export
#' @importFrom stats as.formula model.frame
#' @rdname step_dummy

learn.step_dummy <- function(x, training, ...) {
  col_names <- filter_terms(x$terms, training) 
  
  ## I hate doing this but currently we are going to have 
  ## to save the terms object form the original (= training) 
  ## data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  for(i in seq_along(col_names)) {
    form <- as.formula(paste0("~", col_names[i]))
    terms <- model.frame(
      form, 
      data = training, 
      xlev = x$levels[[i]]
    )
    levels[[i]] <- attr(terms, "terms")
  }
  
  step_dummy_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    contrast = x$contrast,
    naming = x$naming,
    levels = levels
  )
}

#' \code{process.step_dummy} is used to apply the process of creating dummy variables to any data set. This creates new columns in the data set and removes the original column(s). 
#' 
#' @inheritParams process.step_center
#' @param newdata A tibble or data frame that has nominal variables that will be converted to dumy variables.
#' @return \code{process.step_dummy} returns a tibble of processed data. 
#' @export
#' @importFrom stats as.formula model.matrix
#' @importFrom tibble as_tibble
#' @rdname step_dummy

process.step_dummy <- function(object, newdata, ...) {
  ## Maybe do this in C? 
  col_names <- names(object$levels)
  for(i in seq_along(object$levels)) {
    form <- as.formula(paste0("~", object$levels[i]))
    indicators <- model.matrix(
      object = object$levels[[i]], 
      data = newdata
      # contrasts.arg = x$contrast 
    )
    indicators <- indicators[, -1, drop = FALSE]
    ## use backticks for nonstandard factor levels here 
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl)
    newdata <- cbind(newdata, as.data.frame(indicators))
    newdata[, col_names[i]] <- NULL
  }
  as_tibble(newdata)
}

#' @export
print.step_dummy <- function(x, form_width = 30, ...) {
  cat("Dummy variables from ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
