filter_terms <- function(x, ...)
  UseMethod("filter_terms")

## Buckets variables into discrete, mutally exclusive types
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
      POSIXct = "date",
      list = "list"
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

## get variables from formulas
is_formula <- function(x)
  isTRUE(inherits(x, "formula"))

get_lhs_vars <- function(formula, data) {
  if (!is_formula(formula))
    formula <- as.formula(formula)
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  new_formula <- rlang::new_formula(lhs = NULL, rhs = f_lhs(formula))
  get_rhs_vars(new_formula, data)
}

get_rhs_vars <- function(formula, data, no_lhs = FALSE) {
  if (!is_formula(formula)) {
    formula <- as.formula(formula)
  }
  if (no_lhs) {
    formula <- rlang::new_formula(lhs = NULL, rhs = f_rhs(formula))
  }

  ## This will need a lot of work to account for cases with `.`
  ## or embedded functions like `Sepal.Length + poly(Sepal.Width)`.
  ## or should it? what about Y ~ log(x)?
  ## Answer: when called from `form2args`, the function
  ## `inline_check` stops when in-line functions are used.
  data_info <- attr(model.frame(formula, data[1,]), "terms")
  response_info <- attr(data_info, "response")
  predictor_names <- names(attr(data_info, "dataClasses"))
  if (length(response_info) > 0 && all(response_info > 0)) {
    predictor_names <- predictor_names[-response_info]
  }
  predictor_names
}

get_lhs_terms <- function(x) x
get_rhs_terms <- function(x) x



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
#' @param sep A single character value for the separator between the names and
#'  levels.
#'
#' @details When using `dummy_names()`, factor levels that are not valid
#'  variable names (e.g. "some text  with spaces") will be changed to valid
#'  names by [base::make.names()]; see example below. This function will also
#'  change the names of ordinal dummy variables. Instead of values such as
#'  "`.L`", "`.Q`", or "`^4`", ordinal dummy variables are given simple integer
#'  suffixes such as "`_1`", "`_2`", etc.
#'
#' @return `names0` returns a character string of length `num` and
#'  `dummy_names` generates a character vector the same length as
#'  `lvl`.
#' @examples
#' names0(9, "a")
#' names0(10, "a")
#'
#' example <- data.frame(x = ordered(letters[1:5]),
#'                       y = factor(LETTERS[1:5]),
#'                       z = factor(paste(LETTERS[1:5], 1:5)))
#'
#' dummy_names("y", levels(example$y)[-1])
#' dummy_names("z", levels(example$z)[-1])
#'
#' after_mm <- colnames(model.matrix(~x, data = example))[-1]
#' after_mm
#' levels(example$x)
#'
#' dummy_names("x", substring(after_mm, 2), ordinal = TRUE)
#' @export

names0 <- function(num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

#' @export
#' @rdname names0
dummy_names <- function(var, lvl, ordinal = FALSE, sep = "_") {
  # Work around `paste()` recycling bug with 0 length input
  args <- vctrs::vec_recycle_common(var, lvl)
  var <- args[[1]]
  lvl <- args[[2]]

  if(!ordinal)
    nms <- paste(var, make.names(lvl), sep = sep)
  else
    # assuming they are in order:
    nms <- paste0(var, names0(length(lvl), sep))

  nms
}


## As suggested by HW, brought in from the `pryr` package
## https://github.com/hadley/pryr
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is_quosure(f))  {
    fun_calls(quo_get_expr(f))
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
  out <-
    if (is.factor(x)) {
      list(values = levels(x),
           ordered = is.ordered(x),
           factor = TRUE)
    } else {
      list(values = sort(unique(x)),
           ordered = FALSE,
           factor = FALSE)
    }
  out
}

has_lvls <- function(info) {
  !vapply(info, function(x) all(is.na(x$values)), c(logic = TRUE))
}

strings2factors <- function(x, info) {
  check_lvls <- has_lvls(info)
  if (!any(check_lvls)) {
    return(x)
  }
  info <- info[check_lvls]
  vars <- names(info)
  info <- info[vars %in% names(x)]
  for (i in seq_along(info)) {
    lcol <- names(info)[i]
    x[, lcol] <-
      factor(as.character(x[[lcol]]),
             levels = info[[i]]$values,
             ordered = info[[i]]$ordered)
  }
  x
}


# ------------------------------------------------------------------------------

# `complete.cases` fails on list columns. This version counts a list column
# as missing if _all_ values are missing. For if a list vector element is a
# data frame with one missing value, that element of the list column will
# be counted as complete.
n_complete_rows <- function(x) {
  is_list_col <- purrr::map_lgl(x, is.list)
  pos_list_cols <- which(is_list_col)

  for (pos_list_col in pos_list_cols) {
    x[[pos_list_col]] <- purrr::map_lgl(x[[pos_list_col]], flatten_na)
  }

  sum(complete.cases(x))
}

flatten_na <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    FALSE
  }
}

## short summary of training set
train_info <- function(x) {
  data.frame(nrows = nrow(x),
             ncomplete = n_complete_rows(x))
}

# ------------------------------------------------------------------------------



## `merge_term_info` takes the information on the current variable
## list and the information on the new set of variables (after each step)
## and merges them. Special attention is paid to cases where the
## _type_ of data is changed for a common column in the data.


merge_term_info <- function(.new, .old) {
  # Look for conflicts where the new variable type is different from
  # the original value
  .new %>%
    dplyr::rename(new_type = type) %>%
    dplyr::left_join(.old, by = "variable") %>%
    dplyr::mutate(
      type = ifelse(is.na(type), "other", "type"),
      type = ifelse(type != new_type, new_type, type)
    ) %>%
    dplyr::select(-new_type)
}


#' Check for Empty Ellipses
#'
#' @param ... Arguments pass in from a call to `step`
#' @return If not empty, a list of quosures. If empty, an error is thrown.
#' @export
#' @keywords internal
#' @rdname recipes-internal
ellipse_check <- function(...) {
  terms <- quos(...)
  if (is_empty(terms))
    rlang::abort(
      paste0(
      "Please supply at least one variable specification.",
      "See ?selections."
      )
    )
  terms
}

#' Printing Workhorse Function
#'
#' This internal function is used for printing steps.
#'
#' @param tr_obj A character vector of names that have been
#'  resolved during preparing the recipe (e.g. the `columns` object
#'  of [step_log()]).
#' @param untr_obj An object of selectors prior to prepping the
#'  recipe (e.g. `terms` in most steps).
#' @param trained A logical for whether the step has been trained.
#' @param width An integer denoting where the output should be wrapped.
#' @return `NULL`, invisibly.
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
    txt <- format_selectors(untr_obj, width = width)
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
prepare <- function(x, ...)
  rlang::abort(paste0("As of version 0.0.1.9006, used `prep` ",
       "instead of `prepare`"))


#' Check to see if a recipe is trained/prepared
#'
#' @param x A recipe
#' @return A logical which is true if all of the recipe steps have been run
#'  through `prep`. If no steps have been added to the recipe, `TRUE` is
#'  returned only if the recipe has been prepped.
#' @export
#' @examples
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_center(all_numeric())
#'
#' rec %>% fully_trained()
#'
#'
#' rec %>% prep(training = iris) %>% fully_trained()
fully_trained <- function(x) {
  if (is.null(x$steps)) {
    if (any(names(x) == "last_term_info")) {
      res <- TRUE
    } else {
      res <- FALSE
    }
  } else {
    is_tr <- purrr::map_lgl(x$steps, function(x) isTRUE(x$trained))
    res <- all(is_tr)
  }
  res
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
    rlang::abort("Please provide the name of valid step or check (ex: `center`).")
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

#' Quantitatively check on variables
#'
#' This internal function is to be used in the prep function to ensure that
#'   the type of the variables matches the expectation. Throws an error if
#'   check fails.
#' @param dat A data frame or tibble of the training data.
#' @param quant A logical indicating whether the data is expected to be numeric
#'   (TRUE) or a factor/character (FALSE).
#' @export
#' @keywords internal
check_type <- function(dat, quant = TRUE) {
  if (quant) {
    all_good <- vapply(dat, is.numeric, logical(1))
    label <- "numeric"
  } else {
    all_good <- vapply(dat, is_qual, logical(1))
    label <- "factor or character"
  }
  if (!all(all_good))
    rlang::abort(
      paste0(
        "All columns selected for the step",
         " should be ",
         label)
      )
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
  unname(map_chr(x, to_character))
}

to_character <- function(x) {
  if (rlang::is_quosure(x)) {
    res <- rlang::quo_text(x)
  } else {
    res <- as_character(x)
  }
  res
}


simple_terms <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res
}

#' check that newly created variable names don't overlap
#'
#' `check_name` is to be used in the bake function to ensure that
#'   newly created variable names don't overlap with existing names.
#'   Throws an error if check fails.
#' @param res A data frame or tibble of the newly created variables.
#' @param new_data A data frame or tibble passed to the bake function.
#' @param object A trained object passed to the bake function.
#' @param newname A string of variable names if prefix isn't specified
#'   in the trained object.
#' @param names A logical determining if the names should be set using
#' the names function (TRUE) or colnames function (FALSE).
#' @export
#' @keywords internal
check_name <- function(res, new_data, object, newname = NULL, names = FALSE) {
  if(is.null(newname)) {
    newname <- names0(ncol(res), object$prefix)
  }
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% newname
  if(any(intersection)) {
    rlang::abort(
      paste0(
        "Name collision occured in `",
        class(object)[1],
        "`. The following variable names already exists: ",
        paste0(new_data_names[intersection], collapse = ", "),
        "."
      )
    )
  }
  if(names) {
    names(res) <- newname
  } else {
    colnames(res) <- newname
  }
  res
}

#' Make a random identification field for steps
#'
#' @export
#' @param prefix A single character string
#' @param len An integer for the number of random characters
#' @return A character string with the prefix and random letters separated by
#'  and underscore.
#' @keywords internal
rand_id <- function(prefix = "step", len = 5) {
  candidates <- c(letters, LETTERS, paste(0:9))
  paste(prefix,
        paste0(sample(candidates, len, replace = TRUE), collapse = ""),
        sep = "_"
  )
}


check_nominal_type <- function(x, lvl) {
  all_act_cols <- names(x)

  # What columns do we expect to be factors based on the data
  # _before_ the recipes was prepped.

  # Keep in mind that some columns (like outcome data) may not
  # be in the current data so we remove those up-front.
  lvl <- lvl[names(lvl) %in% all_act_cols]

  # Figure out what we expect new data to be:
  fac_ref_cols <- purrr::map_lgl(lvl, function(x) isTRUE(x$factor))
  fac_ref_cols <- names(lvl)[fac_ref_cols]

  if (length(fac_ref_cols) > 0) {

    # Which are actual factors?
    fac_act_cols <- purrr::map_lgl(x, is.factor)

    fac_act_cols <- names(fac_act_cols)[fac_act_cols]

    # There may be some original factors that do not
    was_factor <- fac_ref_cols[!(fac_ref_cols %in% fac_act_cols)]

    if (length(was_factor) > 0) {
      rlang::warn(
        paste0(
          " There ",
          ifelse(length(was_factor) > 1, "were ", "was "),
          length(was_factor),
          ifelse(length(was_factor) > 1, " columns ", " column "),
          "that ",
          ifelse(length(was_factor) > 1, "were factors ", "was a factor "),
          "when the recipe was prepped:\n ",
          paste0("'", was_factor, "'", collapse = ", "),
          ".\n This may cause errors when processing new data."
        )
      )
    }
  }
  invisible(NULL)
}

check_training_set <- function(x, rec, fresh) {
  # In case a variable has multiple roles
  vars <- unique(rec$var_info$variable)

  if (is.null(x)) {
    if (fresh)
      rlang::abort(
        paste0("A training set must be supplied to the `training` argument ",
               "when `fresh = TRUE`."
        )
      )
    x <- rec$template
  } else {
    in_data <- vars %in% colnames(x)
    if (!all(in_data)) {
      rlang::abort(
        paste0("Not all variables in the recipe are present in the supplied ",
               "training set: ",
               paste0("'", vars[!in_data], "'", collapse = ", "),
               "."
        )
      )
    }
    if (!is_tibble(x)) {
      x <- as_tibble(x[, vars, drop = FALSE])
    } else {
      x <- x[, vars]
    }
  }

  steps_trained <- vapply(rec$steps, is_trained, logical(1))
  if (any(steps_trained) & !fresh) {
    if(!rec$retained) {
      rlang::abort(
        paste0(
          "To prep new steps after prepping the original ",
          "recipe, `retain = TRUE` must be set each time that ",
          "the recipe is trained."
        )
      )
    }
    if (!is.null(rec$training)) {
      rlang::warn(
        paste0(
          "The previous data will be used by `prep`; ",
          "the data passed using `training` will be ",
          "ignored."
        )
      )
    }
    x <- rec$template
  }
  x
}

#' Get the `keep_original_cols` value of a recipe step
#'
#' @export
#' @param object A recipe step
#' @return A logical to keep the original variables in the output
#' @keywords internal
get_keep_original_cols <- function(object) {
  # Allow prepping of old recipes created before addition of keep_original_cols
  step_class <- class(object)[1]

  if (is.null(object$keep_original_cols)) {
    ret <- FALSE
    rlang::warn(
      paste0(
        "'keep_original_cols' was added to `",
        step_class,
        "()` after this recipe was created.\n",
        "Regenerate your recipe to avoid this warning."
      )
    )
  } else {
    ret <- object$keep_original_cols
  }

  ret
}

# ------------------------------------------------------------------------------
# From parsnip, keep synced

is_varying <- function(x) {
  if (is.null(x)) {
    res <- FALSE
  } else {
    res <- if (is_quosure(x))
      isTRUE(all.equal(x[[-1]], quote(varying())))
    else
      isTRUE(all.equal(x, quote(varying())))
  }
  res
}

# from tune package
is_tune <- function(x) {
  if (is.call(x)) {
    if (rlang::call_name(x) == "tune") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  FALSE
}

# ------------------------------------------------------------------------------
# For all imputation functions that substitute elements into an existing vector:
# vctrs's cast functions would be better but we'll deal with the known cases
# to avoid a dependency.

cast <- function(x, ref) {
  if (is.factor(ref)) {
    x <- factor(x, levels = levels(ref), ordered = is.ordered(ref))
  } else {
    if (is.integer(ref) & !is.factor(ref)) {
      x <- as.integer(round(x, 0))
    }
  }
  x
}

## -----------------------------------------------------------------------------

print_col_names <- function(x, prefix = "") {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }
  wdth <- options()$width
  if (length(prefix) > 0) {
    prefix <- paste0(prefix, " (", length(x), "): ")
  }
  nm_sdth <- cumsum(c(nchar(prefix), purrr::map_int(x, nchar) + 2))
  keep_x <- nm_sdth + 5 < wdth
  x <- x[ keep_x[-1] ]
  y <- paste0(prefix, paste0(x, collapse = ", "))
  if (!all(keep_x)) {
    y <- paste0(y, ", ...")
  }
  cat(y, "\n", sep = "")
  return(invisible(TRUE))
}

changelog <- function(show, before, after, x) {
  if (!show) {
    return(invisible(TRUE))
  }
  rm_cols <- setdiff(before, after)
  new_cols <- setdiff(after, before)

  cat(class(x)[1], " (", x$id, "): ", sep = "")
  if (length(new_cols) == 0 & length(rm_cols) == 0) {
    cat("same number of columns\n\n")
  } else {
    cat("\n")
    print_col_names(new_cols, " new")
    print_col_names(rm_cols,  " removed")
    cat("\n")
  }
}

