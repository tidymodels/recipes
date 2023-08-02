#' Create traditional dummy variables
#'
#' `step_dummy()` creates a *specification* of a recipe step that will convert
#' nominal data (e.g. characters or factors) into one or more numeric binary
#' model terms corresponding to the levels of the original data.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details. The selected
#'  variables _must_ be factors.
#' @param one_hot A logical. For C levels, should C dummy variables be created
#' rather than C-1?
#' @param preserve Use `keep_original_cols` to specify whether the selected
#'  column(s) should be retained (in addition to the new dummy variables).
#' @param naming A function that defines the naming convention for
#'  new dummy columns. See Details below.
#' @param levels A list that contains the information needed to
#'  create dummy variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_names()]
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
#' @template dummy-naming
#'
#' @details
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
#'  [bake()] will return the data as-is (e.g. with no dummy variables).
#'
#' Note that, by default, the new dummy variable column names obey the naming
#' rules for columns. If there are levels such as "0", [dummy_names()] will put
#' a leading "X" in front of the level (since it uses [make.names()]). This can
#' be changed by passing in a different function to the `naming` argument for
#' this step.
#'
#' Also, there are a number of contrast methods that return fractional values.
#' The columns returned by this step are doubles (not integers).
#'
#' The [package vignette for dummy variables](https://recipes.tidymodels.org/articles/Dummies.html)
#' and interactions has more information.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or original variables selected) and `columns`
#'  (the list of corresponding binary columns) is returned.
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' # Original data: city has 37 levels
#' length(unique(Sacramento$city))
#' unique(Sacramento$city) %>% sort()
#'
#' rec <- recipe(~ city + sqft + price, data = Sacramento)
#'
#' # Default dummy coding: 36 dummy variables
#' dummies <- rec %>%
#'   step_dummy(city) %>%
#'   prep(training = Sacramento)
#'
#' dummy_data <- bake(dummies, new_data = NULL)
#'
#' dummy_data %>%
#'   select(starts_with("city")) %>%
#'   names() # level "anything" is the reference level
#'
#' # Obtain the full set of 37 dummy variables using `one_hot` option
#' dummies_one_hot <- rec %>%
#'   step_dummy(city, one_hot = TRUE) %>%
#'   prep(training = Sacramento)
#'
#' dummy_data_one_hot <- bake(dummies_one_hot, new_data = NULL)
#'
#' dummy_data_one_hot %>%
#'   select(starts_with("city")) %>%
#'   names() # no reference level
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
      lifecycle::deprecate_stop(
        "0.1.16",
        "step_dummy(preserve = )",
        "step_dummy(keep_original_cols = )"
      )
      keep_original_cols <- preserve
    }

    add_step(
      recipe,
      step_dummy_new(
        terms = enquos(...),
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

#' @export
prep.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (length(col_names) > 0) {
    col_names <- check_factor_vars(training, col_names, "step_dummy")

    ## I hate doing this but currently we are going to have
    ## to save the terms object from the original (= training)
    ## data
    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names
    for (i in seq_along(col_names)) {
      form <- rlang::new_formula(lhs = NULL, rhs = rlang::sym(col_names[i]))
      if (x$one_hot) {
        form <- stats::update.formula(form, ~ . -1)
      }
      terms <- model.frame(
        formula = form,
        data = training[1, ],
        xlev = x$levels[[i]],
        na.action = na.pass
      )
      levels[[i]] <- attr(terms, "terms")

      ## About factor levels here: once dummy variables are made,
      ## the `stringsAsFactors` info saved in the recipe (under
      ## recipe$levels will remove the original record of the
      ## factor levels at the end of `prep.recipe` since it is
      ## not a factor anymore. We'll save them here and reset them
      ## in `bake.step_dummy` just prior to calling `model.matrix`
      attr(levels[[i]], "values") <- levels(training[[col_names[i]]])
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

check_factor_vars <- function(data, col_names, step_name, call = caller_env()) {
  fac_check <- vapply(data[, col_names], is.factor, logical(1))
  if (any(!fac_check)) {
    rlang::warn(
      paste0(
        "The following variables are not factor vectors and will be ignored: ",
        paste0("`", names(fac_check)[!fac_check], "`", collapse = ", ")
      )
    )
  }
  col_names <- col_names[fac_check]
  if (length(col_names) == 0) {
    rlang::abort(
      paste0(
        "The `terms` argument in `",
        step_name,
        "` did not select ",
        "any factor columns."
      ),
      call = call
    )
  }
  col_names
}

warn_new_levels <- function(dat, lvl, details = NULL) {
  ind <- which(!(dat %in% lvl))
  if (length(ind) > 0) {
    lvl2 <- unique(dat[ind])
    rlang::warn(
      paste0(
        "There are new levels in a factor: ",
        paste0(lvl2, collapse = ", "),
        details
      )
    )
  }
  invisible(NULL)
}

#' @export
bake.step_dummy <- function(object, new_data, ...) {
  col_names <- names(object$levels)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))

  for (col_name in col_names) {
    levels <- object$levels[[col_name]]
    levels_values <- attr(levels, "values")

    # Make sure that the incoming data has levels consistent with
    # the original (see the note above)
    is_ordered <- attr(levels, "dataClasses") == "ordered"

    if (is.null(levels_values)) {
      rlang::abort("Factor level values not recorded")
    }

    if (length(levels_values) == 1) {
      rlang::abort(
        paste0(
          "Only one factor level in ", col_name, ": ", levels_values
        )
      )
    }

    warn_new_levels(new_data[[col_name]], levels_values)

    new_data[, col_name] <-
      factor(
        new_data[[col_name]],
        levels = levels_values,
        ordered = is_ordered
      )

    indicators <-
      model.frame(
        rlang::new_formula(lhs = NULL, rhs = rlang::sym(col_name)),
        data = new_data[, col_name],
        xlev = levels_values,
        na.action = na.pass
      )

    indicators <-
      model.matrix(
        object = levels,
        data = indicators
      )

    if (!object$one_hot) {
      indicators <- indicators[, colnames(indicators) != "(Intercept)", drop = FALSE]
    }

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^\\`?", col_name, "\\`?"), "", colnames(indicators))
    new_names <- object$naming(col_name, used_lvl, is_ordered)
    colnames(indicators) <- new_names
    indicators <- check_name(indicators, new_data, object, new_names)

    new_data <- vec_cbind(new_data, indicators)
  }

  options(na.action = old_opt)
  on.exit(expr = NULL)

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

print.step_dummy <-
  function(x, width = max(20, options()$width - 20), ...) {
    title <- "Dummy variables from "
    print_step(names(x$levels), x$terms, x$trained, title, width)
    invisible(x)
  }


get_dummy_columns <- function(x, one_hot) {
  x <- attr(x, "values")
  if (!one_hot) x <- x[-1]
  tibble(columns = x)
}


#' @rdname tidy.recipe
#' @export
tidy.step_dummy <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$levels) > 0) {
      res <- purrr::map_dfr(x$levels, get_dummy_columns, x$one_hot, .id = "terms")
    } else {
      res <- tibble(terms = character(), columns = character())
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}
