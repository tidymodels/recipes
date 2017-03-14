


filter_terms <- function(x, ...) UseMethod("filter_terms")


## Buckets variables into discrete, mutally exclusive types
#' @importFrom tibble tibble
get_types <- function(x) {
  type_list <- list(
    nominal = c("character", "factor", "ordered"),
    numeric = c("integer", "numeric", "double"),
    date = c("POSIXct", "Date"),
    censored = "Surv"
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

#' Add a New Step to Current Recipe
#'
#' \code{add_step} adds a step to the last location in the recipe.
#'
#' @param rec A \code{\link{recipe}}.
#' @param object A step object.
#' @keywords datagen
#' @concept preprocessing
#' @return A updated \code{\link{recipe}} with the new step in the last slot for processing.
#' @export
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
#' A General Step Wrapper
#'
#' \code{step} sets the class of the step.
#'
#' @param subclass A character string for the resulting class. For example, if \code{subclass = "blah"} the step object that is returned has class \code{step_blah}.
#' @param ... All arguments to the step that should be returned.
#' @keywords datagen
#' @concept preprocessing
#' @return A updated step with the new class.
#' @export
step <- function(subclass, ...) {
  structure(
    list(
      ...
    ),
    class = c(paste0("step_", subclass), "step")
  )
}



## needs to handle minus and plus signs
## extend to work with variable names
## rewrite print methods

#' @importFrom lazyeval f_rhs
format_formula <- function(x, wdth = options()$width - 9, ...) {
  x <- f_elements(x)
  if(x$signs[1] == "+") x$signs[1] <- ""
  x_items <- unlist(lapply(x$terms, deparse))[-1] # -1 for "list"
  x_items <- paste0(x$signs, x_items)
  format_ch_vec(x_items, width = wdth, sep = " ")
}

## then 9 is to keep space for "[trained]
format_ch_vec <- function(x, sep = ", ", width = options()$width - 9) {
  widths <- nchar(x)
  sep_wd <- nchar(sep)
  adj_wd <- widths+sep_wd
  if(sum(adj_wd) >= width) {
    keepers <- max(which(cumsum(adj_wd) < width)) - 1
    if(length(keepers) == 0 || keepers < 1) {
      x <- paste(length(x), "items")
    } else {
      x <- c(x[1:keepers], "...")
    }
  }
  paste0(x, collapse = sep)
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
    stop("`num` should be > 0", call. = FALSE)
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}



## As suggested by HW, brought in from the `pryr` package
## https://github.com/hadley/pryr
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])

    # Calls inside .Internal are special and shouldn't be included
    if (identical(fname, ".Internal")) return(fname)

    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}

get_levels <- function(x) {
  if(!is.factor(x) & !is.character(x))
    return(NA)
  out <- if(is.factor(x))
    levels(x) else
      sort(unique(x))
  out
}

strings2factors <- function(x, lvl) {
  if(is.null(lvl))
    return(x)
  lvl <- lvl[!is.na(lvl)]
  if(any(!is.na(lvl))) {
   for(i in seq_along(lvl)) {
     lcol <- names(lvl)[i]
     if(!is.factor(x[, lcol]))
       x[, lcol] <- factor(getElement(x, lcol), levels = lvl[[i]])
   }
  }
  x
}



