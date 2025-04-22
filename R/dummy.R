#' Create traditional dummy variables
#'
#' `step_dummy()` creates a *specification* of a recipe step that will convert
#' nominal data (e.g. factors) into one or more numeric binary model terms
#' corresponding to the levels of the original data.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose variables for this step.
#'   See [selections()] for more details. The selected variables _must_ be
#'   factors.
#' @param one_hot A logical. For C levels, should C dummy variables be created
#'   rather than C-1?
#' @param contrasts A named vector or list of contrast functions names. Defaults
#'   to `list(unordered = "contr.treatment", ordered = "contr.poly")`. If only a
#'   single string is passed it will be used for both `unordered` and `ordered`.
#' @param preserve This argument has been deprecated. Please use
#'   `keep_original_cols` instead.
#' @param naming A function that defines the naming convention for new dummy
#'   columns. See Details below.
#' @param levels A list that contains the information needed to create dummy
#'   variables for each variable contained in `terms`. This is `NULL` until the
#'   step is trained by [prep()].
#' @param sparse A single string. Should the columns produced be sparse vectors.
#'   Can take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
#'   then workflows can determine the best option. Defaults to `"auto"`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_names()]
#' @export
#' @details
#'
#' `step_dummy()` will create a set of binary dummy variables from a factor
#' variable. For example, if an unordered factor column in the data set has
#' levels of `"red"`, `"green"`, `"blue"`, the dummy variable bake will create
#' two additional columns of 0/1 data for two of those three values (and remove
#' the original column). For ordered factors, polynomial contrasts are used to
#' encode the numeric values. These defaults are controlled by the `contrasts`
#' argument. Note that since the contrasts are specified via character strings
#' you will need to have those packages loaded. If you are using this with the
#' tune package, you might need to add that these packages to the `pkg` option
#' in `control_grid()`.
#'
#' By default, the excluded dummy variable (i.e. the reference cell) will
#' correspond to the first level of the unordered factor being converted.
#' `step_relevel()` can be used to create a new reference level by setting the
#' `ref_level` argument.
#'
#' @template dummy-naming
#'
#' @details
#'
#' When the factor being converted has a missing value, all of the corresponding
#' dummy variables are also missing. See [step_unknown()] for a solution.
#'
#' When data to be processed contains novel levels (i.e., not contained in the
#' training set), a missing value is assigned to the results. See [step_other()]
#' for an alternative.
#'
#' If no columns are selected (perhaps due to an earlier `step_zv()`), [bake()]
#' will return the data as-is (e.g. with no dummy variables).
#'
#' Note that, by default, the new dummy variable column names obey the naming
#' rules for columns. If there are levels such as `"0"`, [dummy_names()] will
#' put a leading `"X"` in front of the level (since it uses [make.names()]).
#' This can be changed by passing in a different function to the `naming`
#' argument for this step.
#'
#' Also, there are a number of contrast methods that return fractional values.
#' The columns returned by this step are doubles (not integers) when `sparse =
#' FALSE`. The columns returned when `sparse = TRUE` are integers.
#'
#' The [package vignette for dummy
#' variables](https://recipes.tidymodels.org/articles/Dummies.html) and
#' interactions has more information.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `columns` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{columns}{character, names of resulting columns}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-creation
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' # Original data: city has 37 levels
#' length(unique(Sacramento$city))
#' unique(Sacramento$city) |> sort()
#'
#' rec <- recipe(~ city + sqft + price, data = Sacramento)
#'
#' # Default dummy coding: 36 dummy variables
#' dummies <- rec |>
#'   step_dummy(city) |>
#'   prep()
#'
#' dummy_data <- bake(dummies, new_data = NULL)
#'
#' dummy_data |>
#'   select(starts_with("city")) |>
#'   glimpse() # level "anything" is the reference level
#'
#' # Obtain the full set of 37 dummy variables using `one_hot` option
#' dummies_one_hot <- rec |>
#'   step_dummy(city, one_hot = TRUE) |>
#'   prep()
#'
#' dummy_data_one_hot <- bake(dummies_one_hot, new_data = NULL)
#'
#' dummy_data_one_hot |>
#'   select(starts_with("city")) |>
#'   glimpse() # no reference level
#'
#' # Obtain the full set of 37 dummy variables using helmert contrasts
#' dummies_helmert <- rec |>
#'   step_dummy(city, contrasts = "contr.helmert") |>
#'   prep()
#'
#' dummy_data_helmert <- bake(dummies_helmert, new_data = NULL)
#'
#' dummy_data_helmert |>
#'   select(starts_with("city")) |>
#'   glimpse() # no reference level
#'
#' tidy(dummies, number = 1)
#' tidy(dummies_one_hot, number = 1)
#' tidy(dummies_helmert, number = 1)
step_dummy <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    one_hot = FALSE,
    contrasts = list(unordered = "contr.treatment", ordered = "contr.poly"),
    preserve = deprecated(),
    naming = dummy_names,
    levels = NULL,
    sparse = "auto",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("dummy")
  ) {
    if (lifecycle::is_present(preserve)) {
      lifecycle::deprecate_stop(
        "0.1.16",
        "step_dummy(preserve = )",
        "step_dummy(keep_original_cols = )"
      )
    }

    add_step(
      recipe,
      step_dummy_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        one_hot = one_hot,
        contrasts = contrasts,
        preserve = keep_original_cols,
        naming = naming,
        levels = levels,
        sparse = sparse,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_dummy_new <-
  function(
    terms,
    role,
    trained,
    one_hot,
    contrasts,
    preserve,
    naming,
    levels,
    sparse,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "dummy",
      terms = terms,
      role = role,
      trained = trained,
      one_hot = one_hot,
      contrasts = contrasts,
      preserve = preserve,
      naming = naming,
      levels = levels,
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("factor", "ordered"))
  check_bool(x$one_hot, arg = "one_hot")
  check_function(x$naming, arg = "naming", allow_empty = FALSE)
  check_sparse_arg(x$sparse)

  if (is.null(x$contrasts)) {
    x$contrasts <- list(
      unordered = "contr.treatment",
      ordered = "contr.poly"
    )
  }

  if (length(x$contrasts) == 1) {
    x$contrasts <- list(unordered = x$contrasts, ordered = x$contrasts)
  }

  check_contrasts_arg(x$contrasts)

  if (
    !identical(
      getOption("contrasts"),
      c(unordered = "contr.treatment", ordered = "contr.poly")
    )
  ) {
    x$contrasts <- as.list(getOption("contrasts"))

    lifecycle::deprecate_warn(
      "1.3.0",
      I("options(contrasts) with step_dummy()"),
      I("step_dummy(contrasts)")
    )
  }

  if (length(col_names) > 0) {
    ## I hate doing this but currently we are going to have
    ## to save the terms object from the original (= training)
    ## data
    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names

    training_slice <- vctrs::vec_slice(training, 0)

    for (i in seq_along(col_names)) {
      form <- rlang::new_formula(lhs = NULL, rhs = rlang::sym(col_names[i]))
      if (x$one_hot) {
        form <- stats::update.formula(form, ~ . - 1)
      }
      terms <- model.frame(
        formula = form,
        data = training_slice,
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
    contrasts = x$contrasts,
    preserve = x$preserve,
    naming = x$naming,
    levels = levels,
    sparse = x$sparse,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

check_contrasts_arg <- function(x, call = rlang::caller_env()) {
  if (is.vector(x)) {
    if (!identical(sort(names(x)), c("ordered", "unordered"))) {
      offender <- names(x)
      if (length(offender) == 0) {
        cli::cli_abort(
          "The list passed to {.arg contrasts} must have the names
          {.val ordered} and {.val unordered}.",
          call = call
        )
      }
      cli::cli_abort(
        "The names of list passed to {.arg contrasts} must be {.val ordered} and
        {.val unordered}, not {.val {offender}}.",
        call = call
      )
    }
    if (!is.character(x$ordered)) {
      cli::cli_abort(
        "The {.field ordered} element of {.arg contracts} must be a string,
        not {.obj_type_friendly {x$ordered}}.",
        call = call
      )
    }
    if (!is.character(x$unordered)) {
      cli::cli_abort(
        "The {.field unordered} element of {.arg contracts} must be a string,
        not {.obj_type_friendly {x$unordered}}.",
        call = call
      )
    }

    return(NULL)
  }

  cli::cli_abort(
    "{.arg contrasts} must be a named character vector or list,
    not {.obj_type_friendly {x}}.",
    call = call
  )
}

warn_new_levels <- function(dat, lvl, column, step, details = NULL) {
  ind <- which(!(dat %in% lvl))
  if (length(ind) > 0) {
    lvl2 <- unique(dat[ind])
    msg <- c("!" = "There are new levels in {.var {column}}: {.val {lvl2}}.")
    if (anyNA(lvl2)) {
      msg <- c(
        msg,
        "i" = "Consider using {.help [step_unknown()](recipes::step_unknown)} \\
        before {.fn {step}} to handle missing values."
      )
    }
    if (!all(is.na(lvl2))) {
      msg <- c(
        msg,
        "i" = "Consider using {.help [step_novel()](recipes::step_novel)} \\
        before {.fn {step}} to handle unseen values."
      )
    }
    msg <- c(msg, details)

    cli::cli_warn(msg)
  }
  invisible(NULL)
}

#' @export
bake.step_dummy <- function(object, new_data, ...) {
  col_names <- names(object$levels)
  check_new_data(col_names, object, new_data)

  if (is.null(object$contrasts)) {
    object$contrasts <- list(
      unordered = "contr.treatment",
      ordered = "contr.poly"
    )
  }

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

    if (length(levels_values) == 1) {
      cli::cli_abort(
        "Only one factor level in {.var {col_name}}: {.val {levels_values}}."
      )
    }

    warn_new_levels(
      new_data[[col_name]],
      levels_values,
      col_name,
      step = "step_dummy"
    )

    new_data[, col_name] <- factor(
      new_data[[col_name]],
      levels = levels_values,
      ordered = is_ordered
    )

    object$contrasts <- object$contrasts[c("unordered", "ordered")]
    current_contrast <- object$contrasts[is_ordered + 1]
    if (
      !any(
        vapply(
          c("contr.treatment", "contr_one_hot"),
          identical,
          current_contrast[[1]],
          FUN.VALUE = logical(1)
        )
      ) &&
        sparse_is_yes(object$sparse)
    ) {
      object$sparse <- FALSE
    }

    if (sparse_is_yes(object$sparse)) {
      indicators <- sparsevctrs::sparse_dummy(
        x = new_data[[col_name]],
        one_hot = object$one_hot
      )

      indicators <- tibble::new_tibble(indicators)
      used_lvl <- colnames(indicators)
    } else {
      indicators <-
        model.frame(
          rlang::new_formula(lhs = NULL, rhs = rlang::sym(col_name)),
          data = new_data[, col_name],
          xlev = levels_values,
          na.action = na.pass
        )

      current_contrast <- stats::setNames(current_contrast, col_name)

      indicators <- tryCatch(
        model.matrix(
          object = levels,
          data = indicators,
          contrasts.arg = current_contrast
        ),
        error = function(cnd) {
          if (grepl("(vector memory|cannot allocate)", cnd$message)) {
            n_levels <- length(attr(levels, "values"))
            cli::cli_abort(
              "{.var {col_name}} contains too many levels ({n_levels}), \\
              which would result in a data.frame too large to fit in memory.",
              call = NULL
            )
          }
          stop(cnd)
        }
      )

      used_lvl <- attr(indicators, "assign") == 1
      used_lvl <- attr(levels, "values")[used_lvl]

      if (!object$one_hot) {
        indicators <- indicators[,
          colnames(indicators) != "(Intercept)",
          drop = FALSE
        ]
      }
    }

    new_names <- object$naming(col_name, used_lvl, is_ordered)
    colnames(indicators) <- new_names
    indicators <- check_name(indicators, new_data, object, new_names)

    new_data <- vec_cbind(new_data, indicators, .name_repair = "minimal")
  }

  options(na.action = old_opt)
  on.exit(expr = NULL)

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
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
      res <- purrr::map(x$levels, get_dummy_columns, x$one_hot)
      res <- purrr::list_rbind(res, names_to = "terms")
    } else {
      res <- tibble(terms = character(), columns = character())
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' @export
.recipes_estimate_sparsity.step_dummy <- function(x, data, ...) {
  get_levels <- function(x) {
    if (is.factor(x)) {
      return(length(levels(x)))
    } else {
      return(vctrs::vec_unique_count(x))
    }
  }

  n_levels <- lapply(data, get_levels)

  lapply(n_levels, function(n_lvl) {
    c(
      n_cols = ifelse(x$one_hot, n_lvl, n_lvl - 1),
      sparsity = 1 - 1 / n_lvl
    )
  })
}
