filter_terms <- function(x, ...)
  UseMethod("filter_terms")

## Buckets variables into discrete, mutally exclusive types
#' @importFrom tibble tibble
get_types <- function(x) {
  var_types <-
    c(
      character = "nominal",
      factor = "nominal",
      ordered = "nominal",
      integer = "numeric",
      numeric = "numeric",
      double = "numeric",
      Surv = "censored",
      logical = "logical",
      Date = "date",
      POSIXct = "date"
    )

  classes <- lapply(x, class)
  res <- lapply(classes,
                function(x, types) {
                  in_types <- x %in% names(types)
                  if (sum(in_types) > 0) {
                    # not sure what to do with multiple matches; right now
                    ## pick the first match which favors "factor" over "ordered"
                    out <-
                      unname(types[min(which(names(types) %in% x))])
                  } else
                    out <- "other"
                  out
                },
                types = var_types)
  res <- unlist(res)
  tibble(variable = names(res), type = unname(res))
}

type_by_var <- function(classes, dat) {
  res <- sapply(dat, is_one_of, what = classes)
  names(res)[res]
}

is_one_of <- function(x, what) {
  res <- sapply(as.list(what),
                function(class, obj)
                  inherits(obj, what = class),
                obj = x)
  any(res)
}

## get variables from formulas
is_formula <- function(x)
  isTRUE(inherits(x, "formula"))

#' @importFrom rlang f_lhs
get_lhs_vars <- function(formula, data) {
  if (!is_formula(formula))
    formula <- as.formula(formula)
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  new_formula <- as.formula(paste("~", deparse(f_lhs(formula))))
  get_rhs_vars(new_formula, data)
}

#' @importFrom rlang f_rhs
#' @importFrom stats model.frame
get_rhs_vars <- function(formula, data, no_lhs = FALSE) {
  if (!is_formula(formula))
    formula <- as.formula(formula)
  if(no_lhs)
    formula <- as.formula(paste("~", deparse(f_rhs(formula))))

  ## This will need a lot of work to account for cases with `.`
  ## or embedded functions like `Sepal.Length + poly(Sepal.Width)`.
  ## or should it? what about Y ~ log(x)?
  ## Answer: when called from `form2args`, the function
  ## `element_check` stops when in-line functions are used.
  data_info <- attr(model.frame(formula, data), "terms")
  response_info <- attr(data_info, "response")
  predictor_names <- names(attr(data_info, "dataClasses"))
  if (length(response_info) > 0 && all(response_info > 0))
    predictor_names <- predictor_names[-response_info]
  predictor_names
}

get_lhs_terms <- function(x) x
get_rhs_terms <- function(x) x

## ancillary step functions




var_by_role <-
  function(rec,
           role = "predictor",
           returnform = TRUE) {
    res <- rec$var_info$variable[rec$var_info$role == role]
    if (returnform)
      res <- as.formula(paste("~",
                              paste(res, collapse = "+")))
    res
  }


## then 9 is to keep space for "[trained]"
format_ch_vec <-
  function(x,
           sep = ", ",
           width = options()$width - 9) {
    widths <- nchar(x)
    sep_wd <- nchar(sep)
    adj_wd <- widths + sep_wd
    if (sum(adj_wd) >= width) {
      keepers <- max(which(cumsum(adj_wd) < width)) - 1
      if (length(keepers) == 0 || keepers < 1) {
        x <- paste(length(x), "items")
      } else {
        x <- c(x[1:keepers], "...")
      }
    }
    paste0(x, collapse = sep)
  }

format_selectors <- function(x, wdth = options()$width - 9, ...) {
  ## convert to character without the leading ~
  x_items <- lapply(x, function(x)
    as.character(x[-1]))
  x_items <- unlist(x_items)
  format_ch_vec(x_items, width = wdth, sep = ", ")
}

terms.recipe <- function(x, ...)
  x$term_info

filter_terms.formula <- function(formula, data, ...)
  get_rhs_vars(formula, data)


## This function takes the default arguments of `func` and
## replaces them with the matching ones in `options` and
## remove any in `removals`
sub_args <- function(func, options, removals = NULL) {
  args <- formals(func)
  for (i in seq_along(options))
    args[[names(options)[i]]] <- options[[i]]
  if (!is.null(removals))
    args[removals] <- NULL
  args
}
## Same as above but starts with a call object
mod_call_args <- function(cl, args, removals = NULL) {
  if (!is.null(removals))
    for (i in removals)
      cl[[i]] <- NULL
    arg_names <- names(args)
    for (i in arg_names)
      cl[[i]] <- args[[i]]
    cl
}

#' Naming Tools
#'
#' `names0` creates a series of `num` names with a common prefix.
#'  The names are numbered with leading zeros (e.g.
#'  `prefix01`-`prefix10` instead of `prefix1`-`prefix10`).
#'  `dummy_names` can be used for renaming unordered and ordered
#'  dummy variables (in [step_dummy()]).
#'
#' @param num A single integer for how many elements are created.
#' @param prefix A character string that will start each name.
#' @param var A single string for the original factor name.
#' @param lvl A character vectors of the factor levels (in order).
#'  When used with [step_dummy()], `lvl` would be the suffixes
#'  that result _after_ `model.matrix` is called (see the
#'  example below).
#' @param ordinal A logical; was the original factor ordered?
#' @return `names0` returns a character string of length `num` and
#'  `dummy_names` generates a character vector the same length as
#'  `lvl`,
#' @keywords datagen
#' @concept string_functions naming_functions
#' @examples
#' names0(9, "x")
#' names0(10, "x")
#'
#' example <- data.frame(y = ordered(letters[1:5]),
#'                       z = factor(LETTERS[1:5]))
#'
#' dummy_names("z", levels(example$z)[-1])
#'
#' after_mm <- colnames(model.matrix(~y, data = example))[-1]
#' after_mm
#' levels(example$y)
#'
#' dummy_names("y", substring(after_mm, 2), ordinal = TRUE)
#' @export

names0 <- function(num, prefix = "x") {
  if (num < 1)
    stop("`num` should be > 0", call. = FALSE)
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

#' @export
#' @rdname names0
dummy_names <- function(var, lvl, ordinal = FALSE) {
  if(!ordinal)
    nms <- paste(var, make.names(lvl), sep = "_")
  else
    # assuming they are in order:
    nms <- paste0(var, names0(length(lvl), "_"))

  nms
}


## As suggested by HW, brought in from the `pryr` package
## https://github.com/hadley/pryr
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    # Calls inside .Internal are special and shouldn't be included
    if (identical(fname, ".Internal"))
      return(fname)
    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}


get_levels <- function(x) {
  if (!is.factor(x) & !is.character(x))
    return(list(values = NA, ordered = NA))
  out <- if (is.factor(x))
    list(values = levels(x), ordered = is.ordered(x))
  else
    list(values = sort(unique(x)), ordered = FALSE)
  out
}

has_lvls <- function(info)
  !vapply(info, function(x) all(is.na(x$values)), c(logic = TRUE))

strings2factors <- function(x, info) {
  check_lvls <- has_lvls(info)
  if (!any(check_lvls))
    return(x)
  info <- info[check_lvls]
  for (i in seq_along(info)) {
    lcol <- names(info)[i]
    x[, lcol] <- factor(as.character(getElement(x, lcol)),
                        levels = info[[i]]$values,
                        ordered = info[[i]]$ordered)
  }
  x
}

## short summary of training set
train_info <- function(x) {
  data.frame(nrows = nrow(x),
             ncomplete = sum(complete.cases(x)))
}

# Per LH and HW, brought in from the `dplyr` package
is_negated <- function(x) {
  is_lang(x, "-", n = 1)
}

## `merge_term_info` takes the information on the current variable
## list and the information on the new set of variables (after each step)
## and merges them. Special attention is paid to cases where the
## _type_ of data is changed for a common column in the data.

#' @importFrom dplyr left_join
merge_term_info <- function(.new, .old) {
  # Look for conflicts where the new variable type is different from
  # the original value
  tmp_new <- .new
  names(tmp_new)[names(tmp_new) == "type"] <- "new_type"
  tmp <- left_join(tmp_new[, c("variable", "new_type")],
                   .old[, c("variable", "type")],
                   by = "variable")
  tmp <- tmp[!(is.na(tmp$new_type) | is.na(tmp$type)), ]
  diff_type <- !(tmp$new_type == tmp$type)
  if (any(diff_type)) {
    ## Override old type to facilitate merge
    .old$type[which(diff_type)] <- .new$type[which(diff_type)]
  }
  left_join(.new, .old, by = c("variable", "type"))
}


#' Check for Empty Ellipses
#'
#' @param ... Arguments pass in from a call to `step`
#' @return If not empty, a list of quosures. If empty, an error is thrown.
#' @importFrom rlang quos is_empty
#' @export
#' @keywords internal
#' @rdname recipes-internal
ellipse_check <- function(...) {
  terms <- quos(...)
  if (is_empty(terms))
    stop("Please supply at least one variable specification.",
         "See ?selections.",
         call. = FALSE)
  terms
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Printing Workhorse Function
#'
#' This internal function is used for printing steps.
#'
#' @param tr_obj A character vector of names that have been
#'  resolved during preparing the recipe (e.g. the `columns` object
#'  of [step_log()]).
#' @param untr_obj An oject of selectors prior to prepping the
#'  recipe (e.g. `terms` in most steps).
#' @param trained A logical for whether the step has been trained.
#' @param width An integer denoting where the output should be wrapped.
#' @return `NULL``, invisibly.
#' @keywords internal
#' @export
#' @rdname recipes-internal
printer <- function(tr_obj = NULL,
                    untr_obj = NULL,
                    trained = FALSE,
                    width = max(20, options()$width - 30)) {
  if (trained) {
    txt <- format_ch_vec(tr_obj, width = width)
  } else
    txt <- format_selectors(untr_obj, wdth = width)
  if (nchar(txt) == 0)
    txt <- "<none>"
  cat(txt)
  if (trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(NULL)
}


#' @export
#' @keywords internal
#' @rdname recipes-internal
prepare   <- function(x, ...)
  stop("As of version 0.0.1.9006, used `prep` ",
       "instead of `prepare`", call. = FALSE)


#' Check to see if a recipe is trained/prepared
#'
#' @param x A recipe
#' @return A logical which is true if all of the recipe steps have been run
#'  through `prep`. If no steps have been added to the recipe, `TRUE` is
#'  returned.
#' @export
#' @examples
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_center(all_numeric())
#'
#' rec %>% fully_trained
#'
#' rec %>% prep(training = iris) %>% fully_trained
fully_trained <- function(x) {
  if (is.null(x$steps))
    return(TRUE)
  is_tr <- vapply(x$steps, function(x) isTRUE(x$trained), logical(1))
  all(is_tr)
}

#' Detect if a particular step or check is used in a recipe
#'
#' @param recipe A recipe to check.
#' @param name Character name of a step or check, omitted the prefix. That is,
#'   to check if `step_intercept` is present, use `name = intercept`.
#' @return Logical indicating if recipes contains given step.
#' @export
#'
#' @examples
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_intercept()
#'
#' detect_step(rec, "step_intercept")
detect_step <- function(recipe, name) {
  exports <- getNamespaceExports("recipes")
  if (!any(grepl(paste0(".*", name, ".*"), exports)))
    stop("Please provide the name of valid step or check (ex: `center`).",
         call. = FALSE)
  name %in% tidy(recipe)$type
}

# to be used in a recipe
is_skipable <- function(x) {
  if(all("skip" != names(x)))
    return(FALSE)
  else
    return(x$skip)
}

# to be used within a step
skip_me <- function(x) {
  if(!exists("skip"))
    return(FALSE)
  else
    return(x$skip)
}


is_qual <- function(x)
  is.factor(x) | is.character(x)

#' @export
#' @keywords internal
#' @rdname recipes-internal
check_type <- function(dat, quant = TRUE) {
  if (quant) {
    all_good <- vapply(dat, is.numeric, logical(1))
    label <- "numeric"
  } else {
    all_good <- vapply(dat, is_qual, logical(1))
    label <- "factor or character"
  }
  if (!all(all_good))
    stop("All columns selected for the step",
         " should be ", label, call. = FALSE)
  invisible(all_good)
}



## Support functions

#' Check to see if a step or check as been trained
#' @param x a step object.
#' @return A logical
#' @export
#' @keywords internal
#' @rdname recipes-internal
is_trained <- function(x)
  x$trained


#' Convert Selectors to Character
#'
#' This internal function takes a list of selectors (e.g. `terms`
#'  in most steps) and returns a character vector version for
#'  printing.
#' @param x A list of selectors
#' @return A character vector
#' @export
#' @keywords internal
#' @rdname recipes-internal
sel2char <- function(x) {
  term_names <- lapply(x, as.character)
  term_names <-
    vapply(term_names,
           function(x) x[-1],
           character(1))
  term_names
}


simple_terms <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res
}

