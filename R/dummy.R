#' Dummy Variables Creation.
#' 
#' \code{step_dummy} creates a a \emph{specification} of a recipe step that will convert nominal data (e.g. character or factors) into one or more numeric binary model terms for the levels of the original data. For example, if a factor column in the data set has levels of "red", "green", "blue", the dummy variable process will create two additional columns of 0/1 data for two of those three values (and remove the original column).
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to create the dummy variables.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the binary dummy variable columns created by the original variables will be used as predictors in a model. 
#' @param contrast A specification for which type of contrast should be used to make a set of full rank dummy variables. See \code{\link[stats]{contrasts}} for more details. \bold{not currently hooked up}
#' @param naming A function that defines the naming convention for new binary columns. See Details below. 
#' @param levels A list that contains the information needed to create dummy variables for each variable contained in \code{terms}. This is \code{NULL} until the step is trained by \code{\link{learn.recipe}}.
#' @return \code{step_dummy} returns an object of class \code{step_dummy}.
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

#' @importFrom stats as.formula model.frame
learn.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
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

print.step_dummy <- function(x, form_width = 30, ...) {
  cat("Dummy variables from ")
  cat(form_printer(x, wdth = form_width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
