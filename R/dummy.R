#' Dummy Variables Creation
#'
#' \code{step_dummy} creates a a \emph{specification} of a recipe step that
#'   will convert nominal data (e.g. character or factors) into one or more
#'   numeric binary model terms for the levels of the original data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will
#'   be used to create the dummy variables. See \code{\link{selections}} for
#'   more details.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the binary
#'   dummy variable columns created by the original variables will be used as
#'   predictors in a model.
#' @param contrast A specification for which type of contrast should be used
#'   to make a set of full rank dummy variables. See
#'   \code{\link[stats]{contrasts}} for more details. \bold{not currently
#'   working}
#' @param naming A function that defines the naming convention for new binary
#' columns. See Details below.
#' @param levels A list that contains the information needed to create dummy
#'   variables for each variable contained in \code{terms}. This is
#'   \code{NULL} until the step is trained by \code{\link{prep.recipe}}.
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification dummy_variables
#'   variable_encodings
#' @export
#' @details \code{step_dummy} will create a set of binary dummy variables 
#'   from a factor variable. For example, if a factor column in the data set
#'   has levels of "red", "green", "blue", the dummy variable bake will
#'   create two additional columns of 0/1 data for two of those three values
#'   (and remove the original column).
#'
#' By default, the missing dummy variable will correspond to the first level
#'   of the factor being converted.
#'
#' The function allows for non-standard naming of the resulting variables. For
#'   a factor named \code{x}, with levels \code{"a"} and \code{"b"}, the
#'   default naming convention would be to create a new variable called
#'   \code{x_b}. Note that if the factor levels are not valid variable names
#'   (e.g. "some text with spaces"), it will be changed by
#'   \code{\link[base]{make.names}} to be valid (see the example below). The
#'   naming format can be changed using the \code{naming} argument.
#' @examples
#' data(okc)
#' okc <- okc[complete.cases(okc),]
#'
#' rec <- recipe(~ diet + age + height, data = okc)
#'
#' dummies <- rec %>% step_dummy(diet)
#' dummies <- prep(dummies, training = okc)
#'
#' dummy_data <- bake(dummies, newdata = okc)
#'
#' unique(okc$diet)
#' grep("^diet", names(dummy_data), value = TRUE)


step_dummy <- 
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           contrast = options("contrasts"),
           naming = function(var, lvl)
             paste(var, make.names(lvl), sep = "_"),
           levels = NULL) {
  add_step(
    recipe,
    step_dummy_new(
      terms = check_ellipses(...),
      role = role,
      trained = trained,
      contrast = contrast,
      naming = naming,
      levels = levels
    )
  )
}

step_dummy_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           contrast = contrast,
           naming = naming,
           levels = levels
    ) {
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
#' @export
prep.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  ## I hate doing this but currently we are going to have
  ## to save the terms object form the original (= training)
  ## data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  for (i in seq_along(col_names)) {
    form <- as.formula(paste0("~", col_names[i]))
    terms <- model.frame(form,
                         data = training,
                         xlev = x$levels[[i]])
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

#' @export
bake.step_dummy <- function(object, newdata, ...) {
  ## Maybe do this in C?
  col_names <- names(object$levels)
  
  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))
  
  for (i in seq_along(object$levels)) {
    indicators <- 
      model.matrix(
        object = object$levels[[i]],
        data = newdata
      )
    
    options(na.action = old_opt)
    on.exit(expr = NULL)
    
    indicators <- indicators[, -1, drop = FALSE]
    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl)
    newdata <- cbind(newdata, as_tibble(indicators))
    newdata[, col_names[i]] <- NULL
  }
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

print.step_dummy <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Dummy variables from ")
    printer(x$levels, x$terms, x$trained, width = width)
    invisible(x)
  }
