


filter_terms <- function(x, ...) UseMethod("filter_terms")


## Buckets variables into discrete, mutally exclusive types
#' @importFrom tibble tibble
get_types <- function(x) {
  type_list <- list(
    nominal = c("character", "factor", "ordered"),
    numeric = c("integer", "numeric"),
    date = c("POSIXct", "Date")
  )
  res <- lapply(type_list, type_by_var, dat = x)
  ## I think that Hadley has a better function somewhere to do this
  res <- res[sapply(res, length) > 0]
  for(i in seq_along(res))
    res[[i]] <- tibble(variable = res[[i]], type = names(res)[i])
  do.call("rbind", res)
}

type_by_var <- function(classes, dat) {
  res <- sapply(dat, is_one_of, what = classes)
  names(res)[res]
}

is_one_of <- function(x, what) {
  res <- sapply(
    as.list(what),
    function(class, obj) inherits(obj, what = class),
    obj = x
  )
  any(res)
}

## general error trapping functions

check_outcomes_same_type <- function(x) x

## get variables from formulas
is_formula <- function(x)
  isTRUE(inherits(x, "formula"))

#' @importFrom lazyeval f_lhs
get_lhs_vars <- function(formula, data) {
  if(!is_formula(formula))
    formula <- as.formula(formula)
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  formula <- as.formula(paste("~", deparse(f_lhs(formula))))
  get_rhs_vars(formula, data)
}

#' @importFrom stats model.frame
get_rhs_vars <- function(formula, data) {
  if(!is_formula(formula))
    formula <- as.formula(formula)
  ## This will need a lot of work to account for cases with `.`
  ## or embedded functions like `Sepal.Length + poly(Sepal.Width)`.
  ## or should it? what about Y ~ log(x)?
  data_info <- attr(model.frame(formula, data), "terms")
  response_info <- attr(data_info, "response")
  predictor_names <- names(attr(data_info, "dataClasses"))
  if(length(response_info)>0 && all(response_info > 0))
    predictor_names <- predictor_names[-response_info]
  predictor_names
}

get_lhs_terms <- function(x) x
get_rhs_terms <- function(x) x

## ancillary step functions

add_step <- function(rec, object) {
  rec$steps[[length(rec$steps)+1]] <- object
  rec
}


var_by_role <- function(rec, role = "predictor", returnform = TRUE) {
  res <- rec$var_info$variable[rec$var_info$role == role]
  if(returnform)
    res <- as.formula(
      paste("~",
            paste(res, collapse = "+")
      )
    )
  res
}

## Overall wrapper to make new step_X objects
step <- function(subclass, ...) {
  structure(
    list(
      ...
    ),
    class = c(paste0("step_", subclass), "step")
  )
}

#' @importFrom lazyeval f_rhs
form_printer <- function(x, wdth = 50, ...) {
  if(!is_formula(x))
    x <- as.formula(x)
  char_x <- deparse(f_rhs(x))
  if(sum(nchar(char_x)) >= wdth) {
    split_up <- unlist(strsplit(char_x, split = " \\+ "))
    widths <- which(cumsum(nchar(split_up)) <= wdth)
    split_up <- if(length(widths) == length(split_up))
      split_up else c(split_up[widths], "{more}")
    out <- paste0(split_up, collapse = " + ")
  } else out <- char_x
  out
}

terms.recipe <- function(x, ...) x$term_info

filter_terms.formula <- function(formula, data, ...)
  get_rhs_vars(formula, data)


## This function takes the default arguments of `func` and
## replaces them with the matching ones in `options` and
## remove any in `removals`
sub_args <- function(func, options, removals = NULL) {
  args <- formals(func)
  for(i in seq_along(options)) 
    args[[names(options)[i]]] <- options[[i]]
  if(!is.null(removals))
    args[removals] <- NULL
  args
}

#' Sequences of Names with Padded Zeros
#' 
#' This function creates a series of \code{num} names with a common prefix. The names are numbered with leading zeros (e.g. \code{prefix01}-\code{prefix10} instead of \code{prefix1}-\code{prefix10}).
#' 
#' @param num A single integer for how many elements are created.
#' @param prefix A character string that will start each name. .
#' @return A character string of length \code{num}.
#' @keywords datagen
#' @concept string_functions naming_functions
#' @export


names0 <- function(num, prefix = "x") {
  if(num < 1)
    stop("`num` should be > 0")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
