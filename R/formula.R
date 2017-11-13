#' Create a Formula from a Prepared Recipe
#' 
#' In case a model formula is required, the formula method can
#'  be used on a recipe to show what predictors and outcome(s)
#'  could be used. 
#'  
#' @param x A recipe object where all steps have been prepared. 
#' @param ... Note currently used.
#' @return A formula. 
#' @examples 
#' 
#' formula(recipe(Species + Sepal.Length ~ ., data = iris))
#' 
#' iris_rec <- recipe(Species ~ ., data = iris) %>%
#'   step_center(all_numeric()) %>%
#'   prep(training = iris)
#' formula(iris_rec)
#' @export
formula.recipe <- function(x, ...) {
  if (!fully_trained(x))
    stop("All steps in the recipe must be prepped before the ",
         "formula can be computed.",
         call. = FALSE)
  
  x <- summary(x)
  x_vars <- x$variable[x$role == "predictor"]
  y_vars <- x$variable[x$role == "outcome"]
  
  x_vars <- paste0(x_vars, collapse = "+")
  y_vars <- paste0(y_vars, collapse = "+")
  
  as.formula(paste(y_vars, x_vars, sep = "~"))
}
