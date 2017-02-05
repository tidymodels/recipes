
#' @name selections
#' @aliases selections
#' @aliases selection
#' @title Methods for Select Variables in recipe Formulas
#' @description There are a few different methods for selecting variables or model terms using the \code{terms} argument for the \code{step} functions. The two main requirements are that 1) the value of the \code{terms} argument is a formula and 2) that does not contain functions beyond those supported (see below). 
#' 
#' The formula is not processed until the \code{learn} function for the step is executed. Functions can be used inside of the formula that can select columns of the design matrix that may not currently exist. For example, when using \code{step_pca}, the number of columns created by feature extraction may not be known when subsequent steps are defined. In this case, using \code{contains("^PC")} will select all of the columns whose names start with "PC". 
#' 
#' Standard formulas can be used where only the right-hand side is used (e.g. \code{~ x1 + x2 + x3}). Alternatively, select helpers from the \code{dplyr} package, such as \code{\link[dplyr]{starts_with}}, \code{\link[dplyr]{ends_with}}, \code{\link[dplyr]{contains}}, \code{\link[dplyr]{matches}}, \code{\link[dplyr]{num_range}}, \code{\link[dplyr]{everything}}. As an example, \code{~ contains("x") + y + z} is valid. 
#' 
#' However, these are the only functions that can be used in the formula. Using other functions will cause an error, such as  \code{~ contains("x") + y + log(z)}. 
NULL

name_selectors <- c("starts_with", "ends_with", "contains", 
                    "matches", "num_range", "everything")

parse_terms_formula <- function(f, info) {
  vars <- info$variable
  ## split the terms up using +/- as seperators
  elmts <- f_elements(f)
  
  ## TODO determine the sign of the operation
  
  ## Look for inappropriate functions in elements
  check_elements(f)
  
  ## determine if there is a selector involved
  has_func <- has_selector(elmts)
  
  indices <- vector(mode = "list", length = length(elmts)-1)
  
  for(i in seq_along(elmts)[-1]) {
    if(has_func[i-1]) {
      cll <- as.call(elmts[[i]])
      cll$vars <- quote(vars)
      indices[[i-1]] <- eval(cll)
    } else {
      indices[[i-1]] <- which(as.character(elmts[[i]]) == vars)
    }
  }
  indices <- unlist(unique(indices))
  
  ## TODO add/subtract based on sign of elements
  
  if(length(indices) == 0)
    stop("No columns were selected by the `terms` formula for this step.")
  
  vars[indices]
}

f_elements <- function(x) {
  if(recipes:::is_formula(x)) 
  trms <- terms(x)
  attr(trms, "variables")
}

f_signs <- function(x) {
  x 
}

#' @importFrom pryr fun_calls
check_elements <- function(x, allowed = name_selectors) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]
  not_good <- funs[!(funs %in% allowed)]
  if(length(not_good) > 0)
    stop("Only functions allowed are...")
  invisible(NULL)
}

#' @importFrom pryr fun_calls
has_selector <- function(x, allowed = name_selectors) {
  res <- rep(NA, length(x) - 1)
  for(i in 2:length(x)) 
    res[[i-1]] <- isTRUE(fun_calls(x[[i]]) %in% allowed)
  res
}

