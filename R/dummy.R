#' Dummy Variables Creation
#'
#' `step_dummy()` creates a *specification* of a recipe
#'  step that will convert nominal data (e.g. character or factors)
#'  into one or more numeric binary model terms for the levels of
#'  the original data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  _factor_ variables will be used to create the dummy variables. See
#'  [selections()] for more details. The selected
#'  variables must be factors. For the `tidy()` method, these are
#'  not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the binary dummy variable columns created by the original
#'  variables will be used as predictors in a model.
#' @param one_hot A logical. For C levels, should C dummy variables be created
#' rather than C-1?
#' @param preserve Use `keep_original_cols` to specify whether the selected
#'  column(s) should be retained (in addition to the new dummy variables).
#' @param naming A function that defines the naming convention for
#'  new dummy columns. See Details below.
#' @param levels A list that contains the information needed to
#'  create dummy variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep.recipe()].
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or original variables selected) and `columns` (the
#'  list of corresponding binary columns).
#' @keywords datagen
#' @concept preprocessing
#' @concept dummy_variables
#' @concept model_specification
#' @concept dummy_variables
#' @concept variable_encodings
#' @export
#' @details `step_dummy()` will create a set of binary dummy
#'  variables from a factor variable. For example, if an unordered
#'  factor column in the data set has levels of "red", "green",
#'  "blue", the dummy variable bake will create two additional
#'  columns of 0/1 data for two of those three values (and remove
#'  the original column). For ordered factors, polynomial contrasts
#'  are used to encode the numeric values.
#'
#' By default, the excluded dummy variable (i.e. the reference
#'  cell) will correspond to the first level of the unordered
#'  factor being converted.
#'
#' The function allows for non-standard naming of the resulting
#'  variables. For an unordered factor named `x`, with levels `"a"`
#'  and `"b"`, the default naming convention would be to create a
#'  new variable called `x_b`. Note that if the factor levels are
#'  not valid variable names (e.g. "some text with spaces"), it will
#'  be changed by [base::make.names()] to be valid (see the example
#'  below). The naming format can be changed using the `naming`
#'  argument and the function [dummy_names()] is the default. This
#'  function will also change the names of ordinal dummy variables.
#'  Instead of values such as "`.L`", "`.Q`", or "`^4`", ordinal
#'  dummy variables are given simple integer suffixes such as
#'  "`_1`", "`_2`", etc.
#'
#' To change the type of contrast being used, change the global
#' contrast option via `options`.
#'
#' When the factor being converted has a missing value, all of the
#'  corresponding dummy variables are also missing. See [step_unknown()] for
#'  a solution.
#'
#' When data to be processed contains novel levels (i.e., not
#' contained in the training set), a missing value is assigned to
#' the results. See [step_other()] for an alternative.
#'
#' If no columns are selected (perhaps due to an earlier `step_zv()`),
#'  `bake()` will return the data as-is (e.g. with no dummy variables).
#'
#' Note that, by default, the new dummy variable column names obey the naming
#' rules for columns. If there are levels such as "0", [dummy_names()] will put
#' a leading "X" in front of the level (since it uses [make.names()]). This can
#' be changed by passing in a different function to the `naming` argument for
#' this step.
#'
#' The [package vignette for dummy variables](https://recipes.tidymodels.org/articles/Dummies.html)
#' and interactions has more information.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#'  [step_novel()]
#' @examples
#' library(modeldata)
#' data(okc)
#' okc <- okc[complete.cases(okc),]
#'
#' # Original data: diet has 18 levels
#' length(unique(okc$diet))
#' unique(okc$diet) %>% sort()
#'
#' rec <- recipe(~ diet + age + height, data = okc)
#'
#' # Default dummy coding: 17 dummy variables
#' dummies <- rec %>%
#'     step_dummy(diet) %>%
#'     prep(training = okc)
#'
#' dummy_data <- bake(dummies, new_data = NULL)
#'
#' dummy_data %>%
#'     select(starts_with("diet")) %>%
#'     names() # level "anything" is the reference level
#'
#' # Obtain the full set of 18 dummy variables using `one_hot` option
#' dummies_one_hot <- rec %>%
#'     step_dummy(diet, one_hot = TRUE) %>%
#'     prep(training = okc)
#'
#' dummy_data_one_hot <- bake(dummies_one_hot, new_data = NULL)
#'
#' dummy_data_one_hot %>%
#'     select(starts_with("diet")) %>%
#'     names() # no reference level
#'
#'
#' tidy(dummies, number = 1)
#' tidy(dummies_one_hot, number = 1)


step_dummy <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           one_hot = FALSE,
           preserve = deprecated(),
           naming = dummy_names,
           levels = NULL,
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("dummy")) {

    if (lifecycle::is_present(preserve)) {
      lifecycle::deprecate_soft(
        "0.1.16",
        "step_dummy(preserve = )",
        "step_dummy(keep_original_cols = )"
      )
      keep_original_cols <- preserve
    }

    add_step(
      recipe,
      step_dummy_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        one_hot = one_hot,
        preserve = keep_original_cols,
        naming = naming,
        levels = levels,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_dummy_new <-
  function(terms, role, trained, one_hot, preserve, naming, levels,
           keep_original_cols, skip, id) {
    step(
      subclass = "dummy",
      terms = terms,
      role = role,
      trained = trained,
      one_hot = one_hot,
      preserve = preserve,
      naming = naming,
      levels = levels,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

passover <- function(cmd) {
  # cat("`step_dummy()` was not able to select any columns. ",
  #     "No dummy variables will be created.\n")
} # figure out how to return a warning() without exiting

#' @export
prep.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  if (length(col_names) > 0) {
    fac_check <- vapply(training[, col_names], is.factor, logical(1))
    if (any(!fac_check))
      rlang::warn(
        paste0(
        "The following variables are not factor vectors and will be ignored: ",
        paste0("`", names(fac_check)[!fac_check], "`", collapse = ", ")
        )
      )
    col_names <- col_names[fac_check]
    if (length(col_names) == 0) {
      rlang::abort(
        paste0(
        "The `terms` argument in `step_dummy` did not select ",
        "any factor columns."
        )
      )
    }


    ## I hate doing this but currently we are going to have
    ## to save the terms object from the original (= training)
    ## data
    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names
    for (i in seq_along(col_names)) {
      form_chr <- paste0("~", col_names[i])
      if (x$one_hot) {
        form_chr <- paste0(form_chr, "-1")
      }
      form <- as.formula(form_chr)
      terms <- model.frame(form,
                           data = training,
                           xlev = x$levels[[i]],
                           na.action = na.pass)
      levels[[i]] <- attr(terms, "terms")

      ## About factor levels here: once dummy variables are made,
      ## the `stringsAsFactors` info saved in the recipe (under
      ## recipe$levels will remove the original record of the
      ## factor levels at the end of `prep.recipe` since it is
      ## not a factor anymore. We'll save them here and reset them
      ## in `bake.step_dummy` just prior to calling `model.matrix`
      attr(levels[[i]], "values") <-
        levels(getElement(training, col_names[i]))
      attr(levels[[i]], ".Environment") <- NULL
    }
  } else {
    levels <- NULL
  }

  step_dummy_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    one_hot = x$one_hot,
    preserve = x$preserve,
    naming = x$naming,
    levels = levels,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

warn_new_levels <- function(dat, lvl, details = NULL) {
  ind <- which(!(dat %in% lvl))
  if (length(ind) > 0) {
    lvl2 <- unique(dat[ind])
    rlang::warn(
      paste0("There are new levels in a factor: ",
            paste0(lvl2, collapse = ", "),
            details
            )
      )
  }
  invisible(NULL)
}

#' @export
bake.step_dummy <- function(object, new_data, ...) {

  # If no terms were selected
  if (length(object$levels) == 0) {
    return(new_data)
  }

  col_names <- names(object$levels)
  keep_original_cols <- get_keep_original_cols(object)

  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))

  for (i in seq_along(object$levels)) {
    # Make sure that the incoming data has levels consistent with
    # the original (see the note above)
    orig_var <- names(object$levels)[i]
    fac_type <- attr(object$levels[[i]], "dataClasses")

    if (!any(names(attributes(object$levels[[i]])) == "values"))
      rlang::abort("Factor level values not recorded")

    if (length(attr(object$levels[[i]], "values")) == 1)
      rlang::abort(
        paste0("Only one factor level in ", orig_var, ": ",
               attr(object$levels[[i]], "values"))
        )

    warn_new_levels(
      new_data[[orig_var]],
      attr(object$levels[[i]], "values")
    )

    new_data[, orig_var] <-
      factor(getElement(new_data, orig_var),
             levels = attr(object$levels[[i]], "values"),
             ordered = fac_type == "ordered")

    indicators <-
      model.frame(
        as.formula(paste0("~", orig_var)),
        data = new_data[, orig_var],
        xlev = attr(object$levels[[i]], "values"),
        na.action = na.pass
      )

    indicators <-
      model.matrix(
        object = object$levels[[i]],
        data = indicators
      )
    indicators <- as_tibble(indicators)

    options(na.action = old_opt)
    on.exit(expr = NULL)

    if (!object$one_hot) {
      indicators <- indicators[, colnames(indicators) != "(Intercept)", drop = FALSE]
    }

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl, fac_type == "ordered")
    new_data <- bind_cols(new_data, as_tibble(indicators))
    if (any(!object$preserve, !keep_original_cols)) {
      new_data[, col_names[i]] <- NULL
    }
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_dummy <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      if (length(x$levels) > 0) {
        cat("Dummy variables from ")
        cat(format_ch_vec(names(x$levels), width = width))
      } else {
        cat("Dummy variables were *not* created since no columns were selected.")
      }
    } else {
      cat("Dummy variables from ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


get_dummy_columns <- function(x, one_hot) {
  x <- attr(x, "values")
  if (!one_hot) x <- x[-1]
  tibble(columns = x)
}


#' @rdname step_dummy
#' @param x A `step_dummy` object.
#' @export
tidy.step_dummy <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$levels) > 0) {
      res <- purrr::map_dfr(x$levels, get_dummy_columns, x$one_hot, .id = "terms")
    } else {
      res <- tibble(terms = rlang::na_chr, columns = rlang::na_chr)
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}
