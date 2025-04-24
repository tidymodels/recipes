get_lhs_vars <- function(formula, data) {
  if (!rlang::is_formula(formula)) {
    formula <- as.formula(formula)
  }
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  new_formula <- rlang::new_formula(lhs = NULL, rhs = f_lhs(formula))
  get_rhs_vars(new_formula, data)
}

get_rhs_vars <- function(formula, data, no_lhs = FALSE) {
  if (!rlang::is_formula(formula)) {
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

  outcomes_names <- all.names(
    rlang::f_lhs(formula),
    functions = FALSE,
    unique = TRUE
  )

  predictors_names <- all.names(
    rlang::f_rhs(formula),
    functions = FALSE,
    unique = TRUE
  )

  if (any(predictors_names == ".")) {
    predictors_names <- predictors_names[predictors_names != "."]
    predictors_names <- c(predictors_names, colnames(data))
    predictors_names <- unique(predictors_names)
  }

  if (length(predictors_names) > 0 && length(outcomes_names) > 0) {
    predictors_names <- setdiff(predictors_names, outcomes_names)
  }

  predictors_names
}

#' Naming Tools
#'
#' `names0()` creates a series of `num` names with a common prefix. The names
#' are numbered with leading zeros (e.g. `prefix01`-`prefix10` instead of
#' `prefix1`-`prefix10`). `dummy_names` can be used for renaming unordered and
#' ordered dummy variables (in [step_dummy()]).
#'
#' @param num A single integer for how many elements are created.
#' @param prefix A character string that will start each name.
#' @param var A single string for the original factor name.
#' @param lvl A character vectors of the factor levels (in order). When used
#'   with [step_dummy()], `lvl` would be the suffixes that result _after_
#'   `model.matrix` is called (see the example below).
#' @param ordinal A logical; was the original factor ordered?
#' @param sep A single character value for the separator between the names and
#'   levels.
#' @param call The execution environment of a currently running function, e.g.
#'   `caller_env()`. The function will be mentioned in error messages as the
#'   source of the error. See the call argument of [rlang::abort()] for more
#'   information.
#' @details
#'
#' When using `dummy_names()`, factor levels that are not valid variable names
#' (e.g. "some text  with spaces") will be changed to valid names by
#' [base::make.names()]; see example below. This function will also change the
#' names of ordinal dummy variables. Instead of values such as `".L"`, `".Q"`,
#' or `"^4"`, ordinal dummy variables are given simple integer suffixes such as
#' `"_1"`, `"_2"`, etc.
#'
#' @return `names0()` returns a character string of length `num` and
#'   `dummy_names()` generates a character vector the same length as `lvl`.
#'
#' @seealso [developer_functions]
#'
#' @examples
#' names0(9, "a")
#' names0(10, "a")
#'
#' example <- data.frame(
#'   x = ordered(letters[1:5]),
#'   y = factor(LETTERS[1:5]),
#'   z = factor(paste(LETTERS[1:5], 1:5))
#' )
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
names0 <- function(num, prefix = "x", call = rlang::caller_env()) {
  check_number_whole(num, min = 1, call = call)
  ind <- format(seq_len(num))
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

  if (!ordinal) {
    nms <- paste(var, make.names(lvl), sep = sep)
  } else {
    # assuming they are in order:
    nms <- paste0(var, names0(length(lvl), sep))
  }

  nms
}

#' @export
#' @rdname names0
dummy_extract_names <- function(var, lvl, ordinal = FALSE, sep = "_") {
  # Work around `paste()` recycling bug with 0 length input
  args <- vctrs::vec_recycle_common(var, lvl)
  var <- args[[1]]
  lvl <- args[[2]]

  if (!ordinal) {
    nms <- paste(var, make.names(lvl), sep = sep)
  } else {
    # assuming they are in order:
    nms <- paste0(var, names0(length(lvl), sep))
  }

  while (any(duplicated(nms))) {
    dupe_count <- vapply(
      seq_along(nms),
      function(i) {
        sum(nms[i] == nms[1:i])
      },
      1L
    )
    nms[dupe_count > 1] <- paste(
      nms[dupe_count > 1],
      dupe_count[dupe_count > 1],
      sep = sep
    )
  }
  nms
}

fun_calls <- function(f, data) {
  setdiff(all.names(f), colnames(data))
}

get_levels <- function(x) {
  if (!is.factor(x) & !is.character(x)) {
    return(list(values = NA, ordered = NA))
  }
  out <-
    if (is.factor(x)) {
      list(
        values = levels(x),
        ordered = is.ordered(x),
        factor = TRUE
      )
    } else {
      list(
        values = sort(unique(x)),
        ordered = FALSE,
        factor = FALSE
      )
    }
  out
}

has_lvls <- function(info) {
  !vapply(info, function(x) all(is.na(x$values)), c(logic = TRUE))
}

kill_levels <- function(lvls, var_info) {
  vars <- var_info$variable
  roles <- var_info$role
  preds_outcomes <- unique(vars[roles %in% c("outcome", "predictor")])
  others <- unique(setdiff(vars, preds_outcomes))
  if (length(others) > 0L) {
    for (var in others) {
      lvls[[var]] <- list(values = NA, ordered = NA)
    }
  }
  lvls
}

strings2factors <- function(x, info) {
  to_factor <- vapply(
    info,
    function(x) !is.null(x$factor),
    FUN.VALUE = logical(1)
  )
  info <- info[to_factor]
  vars <- names(info)
  info <- info[vars %in% names(x)]
  for (i in seq_along(info)) {
    lcol <- names(info)[i]
    x[, lcol] <-
      factor(
        as.character(x[[lcol]]),
        levels = info[[i]]$values,
        ordered = info[[i]]$ordered,
        exclude = NULL
      )
  }
  x
}

# ------------------------------------------------------------------------------

# `vec_detect_complete()` fails on list columns. This version counts a list
# column as missing if _all_ values are missing. For if a list vector element is
# a data frame with one missing value, that element of the list column will be
# counted as complete.
n_complete_rows <- function(x) {
  is_list_col <- purrr::map_lgl(x, is.list)
  pos_list_cols <- which(is_list_col)

  for (pos_list_col in pos_list_cols) {
    x[[pos_list_col]] <- purrr::map_lgl(x[[pos_list_col]], flatten_na)
  }

  x <- x[, vapply(x, anyNA, logical(1))]

  surv_col_ind <- purrr::map_lgl(x, inherits, "Surv")
  if (any(surv_col_ind)) {
    surv_cols <- stats::complete.cases(x[, surv_col_ind, drop = FALSE])
    non_surv_cols <- vec_detect_complete(x[, !surv_col_ind, drop = FALSE])

    res <- sum(surv_cols & non_surv_cols)
  } else {
    res <- sum(vec_detect_complete(x))
  }

  res
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
  data.frame(
    nrows = nrow(x),
    ncomplete = n_complete_rows(x)
  )
}

# ------------------------------------------------------------------------------

## `merge_term_info` takes the information on the current variable
## list and the information on the new set of variables (after each step)
## and merges them. Special attention is paid to cases where the
## _type_ of data is changed for a common column in the data.

merge_term_info <- function(.new, .old) {
  # Look for conflicts where the new variable type is different from
  # the original value
  .new |>
    dplyr::rename(new_type = type) |>
    dplyr::left_join(.old, by = "variable", multiple = "all") |>
    dplyr::mutate(
      type = ifelse(is.na(type), "other", "type"),
      type = ifelse(type != new_type, new_type, type)
    ) |>
    dplyr::select(-new_type)
}

#' Check for Empty Ellipses
#'
#' `ellipse_check()` is deprecated. Instead, empty selections should be
#' supported by all steps.
#'
#' @param ... Arguments pass in from a call to `step`.
#'
#' @return `ellipse_check()`: If not empty, a list of quosures. If empty, an
#'   error is thrown.
#'
#' @keywords internal
#' @rdname recipes-internal
#' @export
ellipse_check <- function(...) {
  terms <- quos(...)
  if (is_empty(terms)) {
    cli::cli_abort(
      c(
        "!" = "Please supply at least one variable specification.",
        "i" = "See {.help [?selections](recipes::selections)} \\
              for more information."
      )
    )
  }
  terms
}

#' Printing Workhorse Function
#'
#' `printer()` is used for printing steps. `r lifecycle::badge("deprecated")`
#'
#' @param tr_obj A character vector of names that have been resolved during
#'   preparing the recipe (e.g. the `columns` object of [step_log()]).
#' @param untr_obj An object of selectors prior to prepping the recipe (e.g.
#'   `terms` in most steps).
#' @param trained A logical for whether the step has been trained.
#' @param width An integer denoting where the output should be wrapped.
#'
#' @return `printer()`: `NULL`, invisibly.
#'
#' @keywords internal
#' @rdname recipes-internal
#' @export
printer <- function(
  tr_obj = NULL,
  untr_obj = NULL,
  trained = FALSE,
  width = max(20, options()$width - 30)
) {
  lifecycle::deprecate_soft("1.3.0", "printer()", "print_step()")

  if (trained) {
    txt <- format_ch_vec(tr_obj, width = width)
  } else {
    txt <- format_selectors(untr_obj, width = width)
  }

  if (length(txt) == 0L) {
    txt <- "<none>"
  }

  cat(txt)

  if (trained) {
    cat(" [trained]\n")
  } else {
    cat("\n")
  }

  invisible(NULL)
}

#' Check to see if a recipe is trained/prepared
#'
#' @param x A recipe
#' @return A logical which is true if all of the recipe steps have been run
#'   through `prep`. If no steps have been added to the recipe, `TRUE` is
#'   returned only if the recipe has been prepped.
#' @export
#'
#' @seealso [developer_functions]
#'
#' @examples
#' rec <- recipe(Species ~ ., data = iris) |>
#'   step_center(all_numeric())
#'
#' rec |> fully_trained()
#'
#'
#' rec |>
#'   prep(training = iris) |>
#'   fully_trained()
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
#' @seealso [developer_functions]
#'
#' @examples
#' rec <- recipe(Species ~ ., data = iris) |>
#'   step_intercept()
#'
#' detect_step(rec, "intercept")
detect_step <- function(recipe, name) {
  name %in% tidy(recipe)$type
}

# to be used in a recipe
is_skipable <- function(x) {
  if (all("skip" != names(x))) {
    return(FALSE)
  } else {
    return(x$skip)
  }
}

is_qual <- function(x) {
  is.factor(x) | is.character(x)
}

#' Quantitatively check on variables
#'
#' This internal function is to be used in the prep function to ensure that the
#' type of the variables matches the expectation. Throws an error if check
#' fails.
#' @param dat A data frame or tibble of the training data.
#' @param quant A logical indicating whether the data is expected to be numeric
#'   (TRUE) or a factor/character (FALSE). Is ignored if `types` is specified.
#' @param types Character vector of allowed types. Following the same types as
#'   [has_role()]. See details for more.
#'
#' @details
#'
#' Using `types` is a more fine-tuned way to use this. function compared to
#' using `quant`. `types` should specify all allowed types as designated by
#' [.get_data_types]. Suppose you want to allow doubles, integers, characters,
#' factors and ordered factors, then you should specify `types = c("double",
#' "integer", "string", "factor", "ordered")` to get a clear error message.
#'
#' @seealso [developer_functions]
#'
#' @export
#' @keywords internal
check_type <- function(dat, quant = TRUE, types = NULL, call = caller_env()) {
  if (is.null(types)) {
    if (quant) {
      all_good <- vapply(dat, is.numeric, logical(1))
      types <- "numeric"
    } else {
      all_good <- vapply(dat, is_qual, logical(1))
      types <- "factor or character"
    }
  } else {
    all_good <- purrr::map_lgl(get_types(dat)$type, \(.x) any(.x %in% types))
  }

  if (!all(all_good)) {
    info <- get_types(dat)[!all_good, ]
    classes <- map_chr(info$type, function(x) x[1])
    counts <- vctrs::vec_split(info$variable, classes)
    counts$count <- lengths(counts$val)
    counts$text_len <- cli::console_width() -
      18 -
      (counts$count > 1) -
      nchar(counts$key) -
      (counts$count > 2)

    # cli::ansi_collapse() doesn't work for length(x) == 1
    # https://github.com/r-lib/cli/issues/590
    variable_collapse <- function(x, width) {
      x <- paste0("{.var ", x, "}")
      if (length(x) == 1) {
        res <- cli::ansi_strtrim(x, width = width)
      } else if (length(x) == 2) {
        res <- cli::ansi_collapse(
          x,
          last = " and ",
          width = width,
          style = "head"
        )
      } else {
        res <- cli::ansi_collapse(
          x,
          width = width,
          style = "head"
        )
      }
      res
    }

    problems <- glue::glue_data(
      counts,
      "{count} {key} variable{ifelse(count == 1, '', 's')} \\
      found: {purrr::map2_chr(val, text_len, variable_collapse)}"
    )
    names(problems) <- rep("*", length(problems))

    message <- "All columns selected for the step should be {.or {types}}."

    cli::cli_abort(
      c("x" = message, problems),
      call = call
    )
  }

  invisible(all_good)
}

## Support functions

#' Check to see if a step or check as been trained
#'
#' `is_trained()` is a helper function that returned a single logical to
#' indicate whether a recipe is traine or not.
#'
#' @param x a step object.
#' @return `is_trained()`: A single logical.
#'
#' @seealso [developer_functions]
#' @keywords internal
#'
#' @rdname recipes-internal
#' @export
is_trained <- function(x) {
  x$trained
}

#' Convert Selectors to Character
#'
#' `sel2char()` takes a list of selectors (e.g. `terms` in most steps) and
#' returns a character vector version for printing.
#'
#' @param x A list of selectors
#' @return `sel2char()`: A character vector.
#'
#' @seealso [developer_functions]
#'
#' @keywords internal
#' @rdname recipes-internal
#' @export
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
#' `check_name` is to be used in the bake function to ensure that newly created
#' variable names don't overlap with existing names. Throws an error if check
#' fails.
#' @param res A data frame or tibble of the newly created variables.
#' @param new_data A data frame or tibble passed to the bake function.
#' @param object A trained object passed to the bake function.
#' @param newname A string of variable names if prefix isn't specified in the
#'   trained object.
#' @param names A logical determining if the names should be set using the names
#'   function (TRUE) or colnames function (FALSE).
#' @param call The execution environment of a currently running function, e.g.
#'   `caller_env()`. The function will be mentioned in error messages as the
#'   source of the error. See the call argument of [rlang::abort()] for more
#'   information.
#'
#' @seealso [developer_functions]
#'
#' @export
#' @keywords internal
check_name <- function(
  res,
  new_data,
  object,
  newname = NULL,
  names = FALSE,
  call = caller_env()
) {
  if (is.null(newname)) {
    newname <- names0(ncol(res), object$prefix)
  }
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% newname
  if (any(intersection)) {
    nms <- new_data_names[intersection]
    cli::cli_abort(
      c(
        "Name collision occurred. The following variable names already exist:",
        "*" = "{.var {nms}}"
      ),
      call = call
    )
  }
  if (names) {
    names(res) <- newname
  } else {
    colnames(res) <- newname
  }
  res
}

#' Make a random identification field for steps
#'
#'
#' @export
#' @param prefix A single character string
#' @param len An integer for the number of random characters
#' @return A character string with the prefix and random letters separated by
#'   and underscore.
#'
#' @seealso [developer_functions]
#' @keywords internal
rand_id <- function(prefix = "step", len = 5) {
  candidates <- c(letters, LETTERS, paste(0:9))
  paste(
    prefix,
    paste0(sample(candidates, len, replace = TRUE), collapse = ""),
    sep = "_"
  )
}

check_nominal_type <- function(x, lvl, call = rlang::caller_env()) {
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
      cli::cli_warn(
        c(
          "!" = "There {?w/was/were} {length(was_factor)} column{?s} that \\
                {?was a factor/were factors} when the recipe was prepped: \\
                ",
          "*" = "{.and {.var {was_factor}}}",
          "i" = "This may cause errors when processing new data."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

validate_training_data <- function(x, rec, fresh, call = rlang::caller_env()) {
  training_null <- is.null(x)
  if (training_null) {
    if (fresh) {
      cli::cli_abort(
        "A training set must be supplied to the {.arg training} argument \\
        when {.code fresh = TRUE}.",
        call = call
      )
    }
    x <- rec$template
  } else {
    if (is_sparse_matrix(x)) {
      x <- sparsevctrs::coerce_to_sparse_tibble(x, call = call)
    }
    if (!is_tibble(x)) {
      x <- as_tibble(x)
    }
    recipes_ptype_validate(rec, new_data = x, stage = "prep", call = call)

    # In case a variable has multiple roles
    vars <- unique(rec$var_info$variable)
    x <- x[, vars]
  }

  steps_trained <- vapply(rec$steps, is_trained, logical(1))
  if (any(steps_trained) & !fresh) {
    if (!rec$retained) {
      cli::cli_abort(
        "To prep new steps after prepping the original recipe, \\
        {.code retain = TRUE} must be set each time that the recipe is \\
        trained.",
        call = call
      )
    }
    if (!training_null) {
      cli::cli_warn(
        c(
          "!" = "The previous data will be used by {.fn prep}.",
          "i" = "The data passed using {.arg training} will be ignored."
        ),
        call = call
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
#' @seealso [developer_functions]
get_keep_original_cols <- function(object) {
  # Allow prepping of old recipes created before addition of keep_original_cols
  step_class <- class(object)[1]

  if (is.null(object$keep_original_cols)) {
    ret <- FALSE
    cli::cli_warn(
      c(
        "{.arg keep_original_cols} was added to {.fn {step_class}} after this \\
         recipe was created.",
        "i" = "Regenerate your recipe to avoid this warning."
      )
    )
  } else {
    ret <- object$keep_original_cols
  }

  ret
}

# ------------------------------------------------------------------------------

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
  x <- x[keep_x[-1]]
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
    print_col_names(rm_cols, " removed")
    cat("\n")
  }
}

# ------------------------------------------------------------------------------

eval_dimred_call <- function(fn, ...) {
  cl <- rlang::call2(fn, .ns = "dimRed", ...)
  rlang::eval_tidy(cl)
}

dimred_data <- function(dat) {
  cl <- rlang::call2("dimRedData", .ns = "dimRed", rlang::expr(as.matrix(dat)))
  rlang::eval_tidy(cl)
}

uses_dim_red <- function(x) {
  dr <- inherits(x, "dimRedResult")
  if (dr) {
    cli::cli_abort(
      "Recipes version >= 0.1.17 represents the estimates using a different \\
      format. Please recreate this recipe or use version 0.1.16 or less. See \\
      issue {.href [#823](https://github.com/tidymodels/recipes/issues/823)}."
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' Check for required column at bake-time
#'
#' When baking a step, create an information error message when a column that is
#' used by the step is not present in `new_data`.
#'
#' @param req A character vector of required columns.
#' @param object A step object.
#' @param new_data A tibble of data being baked.
#' @return Invisible NULL. Side effects are the focus of the function.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @export
check_new_data <- function(req, object, new_data) {
  if (is.null(req) || length(req) == 0L) {
    return(invisible(NULL))
  }
  col_diff <- setdiff(req, names(new_data))
  if (length(col_diff) == 0) {
    return(invisible(NULL))
  }
  step_cls <- class(object)[1]
  step_id <- object$id
  cli::cli_abort(
    "The following required {cli::qty(col_diff)} column{?s} {?is/are} missing
    from {.arg new_data}: {col_diff}.",
    call = rlang::call2(step_cls)
  )
}

stop_recipes <- function(class = NULL, call = NULL, parent = NULL) {
  rlang::abort(
    class = c(class, "recipes_error"),
    call = call,
    parent = parent
  )
}

stop_recipes_step <- function(call = NULL, parent = NULL) {
  stop_recipes(
    class = "recipes_error_step",
    call = call,
    parent = parent
  )
}

recipes_error_context <- function(expr, step_name) {
  withCallingHandlers(
    expr = force(expr),
    error = function(cnd) {
      stop_recipes_step(
        call = call(step_name),
        parent = cnd
      )
    }
  )
}

#' @method conditionMessage recipes_error
#' @export
conditionMessage.recipes_error <- function(c) {
  rlang::cnd_message(c, prefix = TRUE)
}

vec_paste0 <- function(..., collapse = NULL) {
  args <- vctrs::vec_recycle_common(...)
  rlang::inject(paste0(!!!args, collapse = collapse))
}

#' Removes original columns if options apply
#'
#' This helper function should be used whenever the argument
#' `keep_original_cols` is used in a function.
#'
#' @param new_data A tibble.
#' @param object A step object.
#' @param col_names A character vector, denoting columns to remove.
#' @return new_data with `col_names` removed if `get_keep_original_cols(object)
#'   == TRUE` or `object$preserve == TRUE`.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @export
remove_original_cols <- function(new_data, object, col_names) {
  keep_original_cols <- get_keep_original_cols(object)
  if (any(isFALSE(object$preserve), !keep_original_cols)) {
    new_data <- recipes_remove_cols(new_data, object, col_names)
  }
  new_data
}

#' Removes columns if options apply
#'
#' This helper function removes columns based on character vectors.
#'
#' @param new_data A tibble.
#' @param object A step object.
#' @param col_names A character vector, denoting columns to remove. Will
#'   overwrite `object$removals` if set.
#'
#' @return `new_data` with column names removed if specified by `col_names` or
#'   `object$removals`.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @export
recipes_remove_cols <- function(new_data, object, col_names = character()) {
  if (length(col_names) > 0) {
    removals <- col_names
  } else if (length(object$removals) > 0) {
    removals <- object$removals
  } else {
    return(new_data)
  }

  if (length(removals) > 0) {
    # drop = FALSE in case someone uses this on a data.frame
    new_data <- new_data[, !(colnames(new_data) %in% removals), drop = FALSE]
  }
  new_data
}

#' Role indicators
#'
#' This helper function is meant to be used in `prep()` methods to identify
#' predictors and outcomes by names.
#'
#' @param info data.frame with variable information of columns.
#'
#' @return Character vector of column names.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @name recipes-role-indicator
NULL

#' @rdname recipes-role-indicator
#' @export
recipes_names_predictors <- function(info) {
  get_from_info(info, "predictor")
}

#' @rdname recipes-role-indicator
#' @export
recipes_names_outcomes <- function(info) {
  get_from_info(info, "outcome")
}

get_from_info <- function(info, role) {
  res <- info$variable[info$role == role & !is.na(info$role)]

  res
}

check_zv <- function(data, call = rlang::caller_env()) {
  vz_ind <- vapply(data, one_unique, logical(1))

  if (any(vz_ind)) {
    col_names <- colnames(data)
    offenders <- col_names[vz_ind]
    cli::cli_abort(
      c(
        "!" = "{cli::qty(offenders)} The following column{?s} {?has/have} zero \\
        variance making computations unable to proceed: {offenders}.",
        "i" = "Consider using {.help [?step_zv](recipes::step_zv)} to remove \\
        those columns before this step."
      ),
      call = call
    )
  }
}

try_fetch_eval_tidy <- function(x, call = rlang::caller_env(1)) {
  rlang::try_fetch(
    x,
    error = function(cnd) {
      cli::cli_abort("Failed to compute:", parent = cnd, call = call)
    }
  )
}

#' Check that options argument contain the right elements
#'
#' `check_options()` is to be used in the prep function to ensure that `options`
#' arguments are lists and contain the right elements.
#'
#' @param options options to be checked.
#' @param exclude Character vector, elements that can't be present in `options`.
#' @param include Character vector, Allowed elements in `options`.
#'
#' @seealso [developer_functions]
#'
#' @export
#' @keywords internal
check_options <- function(
  options,
  exclude = NULL,
  include = NULL,
  call = caller_env()
) {
  if (is.null(options)) {
    return(NULL)
  }
  if (identical(options, list())) {
    return(NULL)
  }

  if (!is.list(options)) {
    cli::cli_abort(
      "{.arg options} must be a list, not {.obj_type_friendly {options}}.",
      call = call
    )
  }
  names <- names(options)

  if (is.null(names)) {
    cli::cli_abort(
      "The list passed to {.arg options} must be named.",
      call = call
    )
  }
  if (!is.null(exclude) && any(exclude %in% names)) {
    offenders <- names[exclude %in% names]
    cli::cli_abort(
      "The following element{?s} of the list passed to {.arg options} {?is/are}
      not allowed: {.field {offenders}}.",
      call = call
    )
  }
  if (!is.null(include) && any(!names %in% include)) {
    offenders <- names[!names %in% include]
    cli::cli_abort(
      "{.arg options} must only contain elements {.field {include}}, the
      following are not allowed: {.field {offenders}}.",
      call = call
    )
  }
}

#' Evaluate a selection with tidyselect semantics for arguments
#'
#' @description `recipes_argument_select()` is a variant of
#' [recipes_eval_select()] that is tailored to work well with arguments in steps
#' that specify variables. Such as `denom` in [step_ratio()].
#'
#' This is a developer tool that is only useful for creating new recipes steps.
#'
#' @param quos A list of quosures describing the selection. Captured with
#'   [rlang::enquos()] and stored in the step object corresponding to the
#'   argument.
#'
#' @param data A data frame to use as the context to evaluate the selection in.
#'   This is generally the `training` data passed to the [prep()] method of your
#'   step.
#'
#' @param info A data frame of term information describing each column's type
#'   and role for use with the recipes selectors. This is generally the `info`
#'   data passed to the [prep()] method of your step.
#'
#' @param single A logical. Should an error be thrown if more than 1 variable is
#'   selected. Defaults to `TRUE`.
#'
#' @param arg_name A string. Name of argument, used to enrich error messages.
#'
#' @param call The execution environment of a currently running function, e.g.
#'   `caller_env()`. The function will be mentioned in error messages as the
#'   source of the error. See the call argument of [rlang::abort()] for more
#'   information.
#'
#' @details
#'
#' This function is written to be backwards compatible with previous input types
#' of these arguments. Will thus accept strings, tidyselect, recipes selections,
#' helper functions [imp_vars()] in addition to the prefered bare names.
#'
#' @return
#' A character vector containing the evaluated selection.
#'
#' @seealso [developer_functions]
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(rlang)
#' data(scat, package = "modeldata")
#'
#' rec <- recipe(Species ~ ., data = scat)
#'
#' info <- summary(rec)
#' info
#'
#' recipes_argument_select(quos(Year), scat, info)
#' recipes_argument_select(vars(Year), scat, info)
#' recipes_argument_select(imp_vars(Year), scat, info)
recipes_argument_select <- function(
  quos,
  data,
  info,
  single = TRUE,
  arg_name = "outcome",
  call = caller_env()
) {
  # Because we know the input will be a list of qousures
  expr <- quos[[1]]

  if (quo_is_null(expr)) {
    cli::cli_abort(
      "{.arg {arg_name}} must not be {.code NULL}.",
      call = call
    )
  }

  if (
    rlang::quo_is_call(expr, name = "vars", ns = c("dplyr", "")) ||
      rlang::quo_is_call(expr, name = "imp_vars", ns = c("recipes", "")) ||
      rlang::quo_is_call(expr, name = "denom_vars", ns = c("recipes", ""))
  ) {
    expr <- eval_tidy(expr)

    if (single && length(expr) != 1) {
      cli::cli_abort(
        c(
          x = "only 1 selection is allowed in {.arg {arg_name}},
              not {length(expr)}.",
          i = "For this argument consider using bare names instead."
        ),
        call = call
      )
    }
    expr <- expr(c(!!!expr))
  }

  # Maintain ordering between `data` column names and `info$variable` so
  # `eval_select()` and recipes selectors return compatible positions
  matches <- vctrs::vec_locate_matches(
    names(data),
    info$variable,
    no_match = "error"
  )
  data_info <- vec_slice(info, matches$haystack)

  data_nest <- data_info[names(data_info) != "variable"]
  data_nest <- tibble::new_tibble(data_nest, nrow = vctrs::vec_size(data_nest))

  nested_info <- vctrs::vec_split(data_nest, by = data_info$variable)
  nested_info <- list(variable = nested_info$key, data = nested_info$val)
  nested_info <- tibble::new_tibble(
    nested_info,
    nrow = length(nested_info$variable)
  )
  local_current_info(nested_info)

  out <- tidyselect::eval_select(
    expr,
    data,
    allow_rename = FALSE,
    error_call = call
  )

  if ((single && length(out) != 1) || (!single && length(out) == 0)) {
    cli::cli_abort(
      "only 1 selection is allowed in {.arg {arg_name}}, not {length(out)}.",
      call = call
    )
  }

  out <- names(out)

  if (any(out %in% info$variable[info$role == "case_weights"])) {
    cli::cli_abort(
      "Cannot select case weights variable for {.arg {arg_name}}.",
      call = call
    )
  }

  out
}
