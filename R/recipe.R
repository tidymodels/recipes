#' Create a recipe for preprocessing data
#'
#' A recipe is a description of the steps to be applied to a data set in order
#' to prepare it for data analysis.
#'
#' @param x,data A data frame, tibble, or sparse matrix from the `Matrix`
#'   package of the *template* data set. See [sparse_data] for more information
#'   about use of sparse data. (see below).
#' @param ... Further arguments passed to or from other methods (not currently
#'   used).
#' @param formula A model formula. No in-line functions should be used here
#'   (e.g. `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types
#'   of transformations should be enacted using `step` functions in this
#'   package. Dots are allowed as are simple multivariate outcome terms (i.e. no
#'   need for [cbind()]; see Examples). A model formula may not be the best
#'   choice for high-dimensional data with many columns, because of problems
#'   with memory.
#' @param vars A character string of column names corresponding to variables
#'   that will be used in any context (see below)
#' @param roles A character string (the same length of `vars`) that describes a
#'   single role that the variable will take. This value could be anything but
#'   common roles are `"outcome"`, `"predictor"`, `"case_weight"`, or `"ID"`.
#' @param strings_as_factors A logical, should character columns be converted to
#'   factors? See Details below.
#'
#' @includeRmd man/rmd/recipes.Rmd details
#'
#' @return
#'
#' An object of class `recipe` with sub-objects:
#' \item{var_info}{A tibble containing information about the original data set
#' columns.}
#' \item{term_info}{A tibble that contains the current set of terms in the
#' data set. This initially defaults to the same data contained in
#' `var_info`.}
#' \item{steps}{A list of `step` or `check` objects that define the sequence
#' of preprocessing operations that will be applied to data. The default value
#' is `NULL`.}
#' \item{template}{A tibble of the data. This is initialized to be the same as
#' the data given in the `data` argument but can be different after the recipe
#' is trained.}
#'
#' @seealso [prep()] and [bake()]
#'
#' @examplesIf rlang::is_installed("modeldata")
#' # formula example with single outcome:
#' data(biomass, package = "modeldata")
#'
#' # split data
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' # With only predictors and outcomes, use a formula
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' # Now add preprocessing steps to the recipe
#' sp_signed <- rec |>
#'   step_normalize(all_numeric_predictors()) |>
#'   step_spatialsign(all_numeric_predictors())
#' sp_signed
#'
#' # formula multivariate example:
#' # no need for `cbind(carbon, hydrogen)` for left-hand side
#'
#' multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#' multi_y <- multi_y |>
#'   step_center(all_numeric_predictors()) |>
#'   step_scale(all_numeric_predictors())
#'
#' # example using `update_role` instead of formula:
#' # best choice for high-dimensional data
#'
#' rec <- recipe(biomass_tr) |>
#'   update_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
#'     new_role = "predictor"
#'   ) |>
#'   update_role(HHV, new_role = "outcome") |>
#'   update_role(sample, new_role = "id variable") |>
#'   update_role(dataset, new_role = "splitting indicator")
#' rec
#' @export
recipe <- function(x, ...) {
  UseMethod("recipe")
}

#' @rdname recipe
#' @export
recipe.default <- function(x, ...) {
  # Doing this here since it should work for all types of Matrix classes
  if (is_sparse_matrix(x)) {
    x <- sparsevctrs::coerce_to_sparse_tibble(x, call = caller_env(0))
    return(recipe(x, ...))
  }

  cli::cli_abort(
    c(
      x = "{.arg x} should be a data frame, matrix, formula, or tibble.",
      i = "{.arg x} is {.obj_type_friendly {x}}."
    )
  )
}

#' @rdname recipe
#' @export
recipe.data.frame <-
  function(
    x,
    formula = NULL,
    ...,
    vars = NULL,
    roles = NULL,
    strings_as_factors = NULL
  ) {
    if (!is.null(formula)) {
      if (!is.null(vars)) {
        cli::cli_abort(
          "The {.arg vars} argument will be ignored when a formula is used."
        )
      }
      if (!is.null(roles)) {
        cli::cli_abort(
          "The {.arg roles} argument will be ignored when a formula is used."
        )
      }

      obj <- recipe.formula(formula, x, ...)
      return(obj)
    }

    if (is.null(vars)) {
      vars <- colnames(x)
    }

    if (!is_tibble(x)) {
      x <- as_tibble(x)
    }

    if (any(table(vars) > 1)) {
      offenders <- vctrs::vec_count(vars)
      offenders <- offenders$key[offenders$count != 1]

      cli::cli_abort(
        c(
          x = "{.arg vars} must have unique values.",
          i = "The following values were duplicated: {.and {.field {offenders}}}."
        )
      )
    }
    if (any(!(vars %in% colnames(x)))) {
      offenders <- vars[!(vars %in% colnames(x))]

      cli::cli_abort(
        c(
          x = "The following elements of {.arg vars} are not found in {.arg x}:",
          "*" = "{.and {.field {offenders}}}."
        )
      )
    }

    x <- x[, vars]

    var_info <- tibble(variable = vars)

    ## Check and add roles when available
    if (!is.null(roles)) {
      if (length(roles) != length(vars)) {
        cli::cli_abort(
          c(
            x = "{.arg vars} and {.arg roles} must have same length.",
            "*" = "{.arg vars} has length {length(vars)}",
            "*" = "{.arg roles} has length {length(roles)}"
          )
        )
      }
      var_info$role <- roles
    } else {
      var_info$role <- NA_character_
    }

    ## Add types
    var_info <- full_join(get_types(x), var_info, by = "variable")
    var_info$source <- "original"

    # assign case weights
    case_weights_cols <- map_lgl(x, hardhat::is_case_weights)
    case_weights_n <- sum(case_weights_cols, na.rm = TRUE)
    if (case_weights_n > 1) {
      too_many_case_weights(names(case_weights_cols)[case_weights_cols])
    }
    var_info$role[case_weights_cols] <- "case_weights"

    requirements <- new_role_requirements()

    ## Return final object of class `recipe`
    out <- list(
      var_info = var_info,
      term_info = var_info,
      steps = NULL,
      template = x,
      levels = NULL,
      retained = NA,
      requirements = requirements,
      ptype = vctrs::vec_ptype(x),
      strings_as_factors = strings_as_factors
    )
    class(out) <- "recipe"
    out
  }

#' @rdname recipe
#' @export
recipe.formula <- function(formula, data, ...) {
  if (rlang::is_missing(data)) {
    cli::cli_abort("{.arg data} is missing with no default.")
  }

  if (!is.data.frame(data) && !is.matrix(data) && !is_sparse_matrix(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, matrix, or sparse matrix,
      not {.obj_type_friendly {data}}."
    )
  }

  # check for minus:
  f_funcs <- fun_calls(formula, data)
  if (any(f_funcs == "-")) {
    cli::cli_abort(
      c(
        "x" = "{.code -} is not allowed in a recipe formula.",
        "i" = "Use {.help [{.fun step_rm}](recipes::step_rm)} instead."
      )
    )
  }

  if (is_sparse_matrix(data)) {
    data <- sparsevctrs::coerce_to_sparse_tibble(data, call = caller_env(0))
  }

  # Dealing with sf data sets
  if (rlang::inherits_any(data, "sf")) {
    class(data) <- setdiff(class(data), "sf")
  }

  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }

  # Check for other in-line functions
  args <- form2args(formula, data, ...)
  obj <- recipe.data.frame(
    x = args$x,
    formula = NULL,
    ...,
    vars = args$vars,
    roles = args$roles
  )
  obj
}

#' @rdname recipe
#' @export
recipe.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  recipe.data.frame(x, ...)
}

form2args <- function(formula, data, ..., call = rlang::caller_env()) {
  if (!rlang::is_formula(formula)) {
    formula <- as.formula(formula)
  }

  ## check for in-line formulas
  inline_check(formula, data, call)

  ## use rlang to get both sides of the formula
  outcomes <- get_lhs_vars(formula, data)
  predictors <- get_rhs_vars(formula, data, no_lhs = TRUE)

  ## if . was used on the rhs, subtract out the outcomes
  predictors <- predictors[!(predictors %in% outcomes)]

  ## get `vars` from lhs and rhs of formula
  vars <- c(predictors, outcomes)

  ## subset data columns
  data <- data[, vars]

  ## derive roles
  roles <- rep("predictor", length(predictors))
  if (length(outcomes) > 0) {
    roles <- c(roles, rep("outcome", length(outcomes)))
  }

  # assign case weights
  case_weights_cols <- map_lgl(data, hardhat::is_case_weights)
  case_weights_n <- sum(case_weights_cols, na.rm = TRUE)
  if (case_weights_n > 1) {
    too_many_case_weights(
      names(case_weights_cols)[case_weights_cols],
      call = call
    )
  }
  roles[case_weights_cols] <- "case_weights"

  ## pass to recipe.default with vars and roles

  list(x = data, vars = vars, roles = roles)
}

inline_check <- function(x, data, call) {
  funs <- fun_calls(x, data)
  funs <- funs[!(funs %in% c("~", "+", "-", "."))]

  if (length(funs) > 0) {
    cli::cli_abort(
      c(
        x = "Misspelled variable name or in-line functions detected.",
        i = "{cli::qty(length(funs))}The following function{?s}/misspelling{?s} \\
          {?was/were} found: {.and {.code {funs}}}.",
        i = "Use steps to do transformations instead.",
        i = "If your modeling engine uses special terms in formulas, pass \\
          that formula to workflows as a \\
          {.help [model formula](parsnip::model_formula)}."
      ),
      call = call
    )
  }

  invisible(x)
}

#' Estimate a preprocessing recipe
#'
#' For a recipe with at least one preprocessing operation, estimate the required
#' parameters from a training set that can be later applied to other data sets.
#'
#' @param x an [recipe()] object.
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @param training A data frame, tibble, or sparse matrix from the `Matrix`
#'   package, that will be used to estimate parameters for preprocessing. See
#'   [sparse_data] for more information about use of sparse data.
#' @param fresh A logical indicating whether already trained operation should be
#'   re-trained. If `TRUE`, you should pass in a data set to the argument
#'   `training`.
#' @param verbose A logical that controls whether progress is reported as
#'   operations are executed.
#' @param retain A logical: should the *preprocessed* training set be saved into
#'   the `template` slot of the recipe after training? This is a good idea if
#'   you want to add more steps later but want to avoid re-training the existing
#'   steps. Also, it is advisable to use `retain = TRUE` if any steps use the
#'   option `skip = FALSE`. **Note** that this can make the final recipe size
#'   large. When `verbose = TRUE`, a message is written with the approximate
#'   object size in memory but may be an underestimate since it does not take
#'   environments into account.
#' @param log_changes A logical for printing a summary for each step regarding
#'   which (if any) columns were added or removed during training.
#' @param strings_as_factors A logical: should character columns that have role
#'   `"predictor"` or `"outcome"` be converted to factors? **This option has now
#'   been moved to [recipe()]**; please specify `strings_as_factors` there and
#'   see the notes in the Details section for that function.
#'
#' @details
#'
#' Given a data set, this function estimates the required quantities and
#' statistics needed by any operations. [prep()] returns an updated recipe with
#' the estimates. If you are using a recipe as a preprocessor for modeling, we
#' **highly recommend** that you use a `workflow()` instead of manually
#' estimating a recipe (see the example in [recipe()]).
#'
#' Note that missing data is handled in the steps; there is no global `na.rm`
#' option at the recipe level or in [prep()].
#'
#' Also, if a recipe has been trained using [prep()] and then steps are added,
#' [prep()] will only update the new operations. If `fresh = TRUE`, all of the
#' operations will be (re)estimated.
#'
#' As the steps are executed, the `training` set is updated. For example, if the
#' first step is to center the data and the second is to scale the data, the
#' step for scaling is given the centered data.
#'
#' @return
#'
#' A recipe whose step objects have been updated with the required quantities
#' (e.g. parameter estimates, model objects, etc). Also, the `term_info` object
#' is likely to be modified as the operations are executed.
#'
#' @seealso [recipe()] and [bake()]
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' library(dplyr)
#'
#' ames <- mutate(ames, Sale_Price = log10(Sale_Price))
#'
#' ames_rec <-
#'   recipe(
#'     Sale_Price ~ Longitude + Latitude + Neighborhood + Year_Built + Central_Air,
#'     data = ames
#'   ) |>
#'   step_other(Neighborhood, threshold = 0.05) |>
#'   step_dummy(all_nominal()) |>
#'   step_interact(~ starts_with("Central_Air"):Year_Built) |>
#'   step_ns(Longitude, Latitude, deg_free = 5)
#'
#' prep(ames_rec, verbose = TRUE)
#'
#' prep(ames_rec, log_changes = TRUE)
#' @export
prep <- function(x, ...) {
  UseMethod("prep")
}

#' @rdname prep
#' @export
prep.recipe <-
  function(
    x,
    training = NULL,
    fresh = FALSE,
    verbose = FALSE,
    retain = TRUE,
    log_changes = FALSE,
    strings_as_factors = TRUE,
    ...
  ) {
    training <- validate_training_data(training, x, fresh)

    tr_data <- train_info(training)

    # Record the original levels for later checking
    orig_lvls <- lapply(training, get_levels)

    if (!missing(strings_as_factors)) {
      lifecycle::deprecate_soft(
        when = "1.3.0",
        what = "recipes::prep.recipe(strings_as_factors)",
        with = "recipes::recipe(strings_as_factors)"
      )
    }

    if (!is.null(x$strings_as_factors)) {
      strings_as_factors <- x$strings_as_factors
    }

    if (strings_as_factors) {
      lvls <- lapply(training, get_levels)
      lvls <- kill_levels(lvls, x$var_info)
      training <- strings2factors(training, lvls)
    } else {
      lvls <- NULL
    }

    # The only way to get the results for skipped steps is to
    # use `retain = TRUE` so issue a warning if this is not the case
    skippers <- map_lgl(x$steps, is_skipable)
    if (any(skippers) & !retain) {
      cli::cli_warn(
        "Since some operations have {.code skip = TRUE}, using \\
        {.code retain = TRUE} will allow those step's results to be accessible."
      )
    }

    # Recalculate types of old recipes (>= 1.0.1) if possible and necessary
    if (
      all(x$var_info$source == "original") &
        inherits(x$var_info$type, "character")
    ) {
      x$var_info <- x$var_info |>
        dplyr::select(-type) |>
        dplyr::left_join(
          get_types(training),
          by = "variable",
          multiple = "all"
        ) |>
        dplyr::select(variable, type, role, source)
    }

    if (
      all(x$term_info$source == "original") &
        inherits(x$term_info$type, "character")
    ) {
      x$term_info <- x$term_info |>
        dplyr::select(-type) |>
        dplyr::left_join(
          get_types(training),
          by = "variable",
          multiple = "all"
        ) |>
        dplyr::select(variable, type, role, source)
    }

    if (fresh) {
      x$term_info <- x$var_info
    }

    fit_times <- list()

    running_info <- x$term_info |> mutate(number = 0, skip = FALSE)

    get_needs_tuning <- function(x) {
      res <- map_lgl(x, is_tune)
      res <- names(res)[res]
      res <- vctrs::vec_recycle_common(step = class(x)[[1L]], arg = res)
      tibble::new_tibble(res)
    }

    needs_tuning <- purrr::map(x$steps, get_needs_tuning)
    needs_tuning <- purrr::list_rbind(needs_tuning)

    if (nrow(needs_tuning) > 0) {
      args <- vctrs::vec_split(needs_tuning$arg, needs_tuning$step)
      msg <- c(
        x = "You cannot {.fun prep} a tunable recipe.",
        i = "{cli::qty(nrow(args))}The following step{?s} \\
             {?no/has/have} {.fun tune}:"
      )

      step_msg <- paste0(
        "{needs_tuning$step[",
        seq_len(nrow(needs_tuning)),
        "]}: {.and {.arg {needs_tuning$arg[",
        seq_len(nrow(needs_tuning)),
        "]}}}"
      )
      names(step_msg) <- rep("*", nrow(needs_tuning))

      cli::cli_abort(c(msg, step_msg))
    }

    for (i in seq(along.with = x$steps)) {
      step_name <- class(x$steps[[i]])[[1L]]

      note <- paste("oper", i, gsub("_", " ", step_name))
      if (!x$steps[[i]]$trained | fresh) {
        if (verbose) {
          cat(note, "[training]", "\n")
        }

        before_nms <- names(training)

        # Compute anything needed for the preprocessing steps
        # then apply it to the current training set
        time <- proc.time()
        x$steps[[i]] <- recipes_error_context(
          prep(x$steps[[i]], training = training, info = x$term_info),
          step_name = step_name
        )
        prep_time <- proc.time() - time

        time <- proc.time()
        training <- recipes_error_context(
          bake(x$steps[[i]], new_data = training),
          step_name = step_name
        )
        bake_time <- proc.time() - time

        fit_times[[i]] <- list(
          stage_id = paste(c("prep", "bake"), x$steps[[i]]$id, sep = "."),
          elapsed = c(prep_time[["elapsed"]], bake_time[["elapsed"]])
        )

        if (!is_tibble(training)) {
          cli::cli_abort(
            c(
              "x" = "{.fun bake} methods should always return tibbles.",
              "i" = "{.fun {paste0('bake.', step_name)}} returned \\
                   {.obj_type_friendly {training}}."
            )
          )
        }
        x$term_info <- merge_term_info(get_types(training), x$term_info)

        # Update the roles and the term source
        if (!is.na(x$steps[[i]]$role)) {
          new_vars <- setdiff(x$term_info$variable, running_info$variable)
          pos_new_var <- x$term_info$variable %in% new_vars
          pos_new_and_na_role <- pos_new_var & is.na(x$term_info$role)
          pos_new_and_na_source <- pos_new_var & is.na(x$term_info$source)

          x$term_info$role[pos_new_and_na_role] <- x$steps[[i]]$role
          x$term_info$source[pos_new_and_na_source] <- "derived"
        }

        changelog(log_changes, before_nms, names(training), x$steps[[i]])

        running_info <- rbind(
          running_info,
          mutate(x$term_info, number = i, skip = x$steps[[i]]$skip)
        )
      } else {
        if (verbose) cat(note, "[pre-trained]\n")
      }
    }

    ## The steps may have changed the data so reassess the levels
    if (strings_as_factors) {
      lvls <- lapply(training, get_levels)
      lvls <- kill_levels(lvls, x$term_info)
      check_lvls <- has_lvls(lvls)
      if (!any(check_lvls)) lvls <- NULL
    } else {
      lvls <- NULL
    }

    if (retain) {
      if (verbose) {
        cat(
          "The retained training set is ~",
          format(object.size(training), units = "Mb", digits = 2),
          " in memory.\n\n"
        )
      }

      x$template <- training
    } else {
      x$template <- training[0, ]
    }

    x$tr_info <- tr_data
    x$levels <- lvls
    x$orig_lvls <- orig_lvls
    x$retained <- retain
    x$fit_times <- dplyr::bind_rows(fit_times)
    # In case a variable was removed, and that removal step used
    # `skip = TRUE`, we need to retain its record so that
    # selectors can be properly used with `bake`. This tibble
    # captures every variable originally in the data or that was
    # created along the way. `number` will be the last step where
    # that variable was available.
    x$last_term_info <-
      running_info |>
      group_by(variable) |>
      arrange(desc(number)) |>
      summarise(
        type = list(dplyr::first(type)),
        role = list(unique(unlist(role))),
        source = dplyr::first(source),
        number = dplyr::first(number),
        skip = dplyr::first(skip),
        .groups = "keep"
      )
    x
  }

#' Apply a trained preprocessing recipe
#'
#' For a recipe with at least one preprocessing operation that has been trained
#' by [prep()], apply the computations to new data.
#'
#' @param object A trained object such as a [recipe()] with at least one
#'   preprocessing operation.
#' @param ... One or more selector functions to choose which variables will be
#'   returned by the function. See [selections()] for more details. If no
#'   selectors are given, the default is to use [dplyr::everything()].
#' @param new_data A data frame, tibble, or sparse matrix from the `Matrix`
#'   package for whom the preprocessing will be applied. If `NULL` is given to
#'   `new_data`, the pre-processed _training data_ will be returned (assuming
#'   that `prep(retain = TRUE)` was used). See [sparse_data] for more
#'   information about use of sparse data.
#' @param composition Either `"tibble"`, `"matrix"`, `"data.frame"`, or
#'   `"dgCMatrix"``for the format of the processed data set. Also, note that
#'   this argument should be called **after** any selectors and the selectors
#'   should only resolve to numeric columns if `composition` is set to
#'   `"matrix"` or `"dgCMatrix"`. If the data contains sparse columns they will
#'   be perseved for `"tibble"` and `"data.frame"`, and efficiently used for
#'   `"dgCMatrix"`.
#'
#' @details
#'
#' [bake()] takes a trained recipe and applies its operations to a data set to
#' create a design matrix. If you are using a recipe as a preprocessor for
#' modeling, we **highly recommend** that you use a `workflow()` instead of
#' manually applying a recipe (see the example in [recipe()]).
#'
#' If the data set is not too large, time can be saved by using the `retain =
#' TRUE` option of [prep()]. This stores the processed version of the training
#' set. With this option set, `bake(object, new_data = NULL)` will return it for
#' free.
#'
#' Also, any steps with `skip = TRUE` will not be applied to the data when
#' [bake()] is invoked with a data set in `new_data`. `bake(object, new_data =
#' NULL)` will always have all of the steps applied.
#'
#' @return
#'
#' A tibble, matrix, or sparse matrix that may have different columns than the
#' original columns in `new_data`.
#'
#' @seealso [recipe()] and [prep()]
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' ames <- mutate(ames, Sale_Price = log10(Sale_Price))
#'
#' ames_rec <-
#'   recipe(Sale_Price ~ ., data = ames[-(1:6), ]) |>
#'   step_other(Neighborhood, threshold = 0.05) |>
#'   step_dummy(all_nominal()) |>
#'   step_interact(~ starts_with("Central_Air"):Year_Built) |>
#'   step_ns(Longitude, Latitude, deg_free = 2) |>
#'   step_zv(all_predictors()) |>
#'   prep()
#'
#' # return the training set (already embedded in ames_rec)
#' bake(ames_rec, new_data = NULL)
#'
#' # apply processing to other data:
#' bake(ames_rec, new_data = head(ames))
#'
#' # only return selected variables:
#' bake(ames_rec, new_data = head(ames), all_numeric_predictors())
#' bake(ames_rec, new_data = head(ames), starts_with(c("Longitude", "Latitude")))
#' @export
bake <- function(object, ...) {
  UseMethod("bake")
}

#' @rdname bake
#' @export
bake.recipe <- function(object, new_data, ..., composition = "tibble") {
  if (rlang::is_missing(new_data)) {
    cli::cli_abort(
      "{.arg new_data} must be either a data frame or NULL. \\
      No value is not allowed."
    )
  }

  if (is.null(new_data)) {
    return(juice(object, ..., composition = composition))
  }

  if (!fully_trained(object)) {
    cli::cli_abort(
      c(
        "x" = "At least one step has not been trained.",
        "i" = "Please run {.help [{.fun prep}](recipes::prep)}."
      )
    )
  }

  if (!any(composition == formats)) {
    cli::cli_abort(
      c(
        "x" = "{.arg composition} cannot be {.val {composition}}.",
        "i" = "Allowed values are {.or {.val {formats}}}."
      )
    )
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  if (is_sparse_matrix(new_data)) {
    new_data <- sparsevctrs::coerce_to_sparse_tibble(
      new_data,
      call = caller_env(0)
    )
  }

  if (!is_tibble(new_data)) {
    new_data <- as_tibble(new_data)
  }

  check_role_requirements(object, new_data)

  check_nominal_type(new_data, object$orig_lvls)

  # Drop completely new columns from `new_data` and reorder columns that do
  # still exist to match the ordering used when training
  original_names <- names(new_data)
  original_training_names <- unique(object$var_info$variable)
  bakeable_names <- intersect(original_training_names, original_names)
  new_data <- new_data[, bakeable_names]

  n_steps <- length(object$steps)

  for (i in seq_len(n_steps)) {
    step <- object$steps[[i]]

    if (is_skipable(step)) {
      next
    }

    new_data <- bake(step, new_data = new_data)

    if (!is_tibble(new_data)) {
      step_name <- attr(step, "class")[1]

      cli::cli_abort(
        c(
          "x" = "{.fun bake} methods should always return tibbles.",
          "i" = "{.fun {paste0('bake.', step_name)}} returned \\
                   {.obj_type_friendly {new_data}}."
        )
      )
    }
  }

  # Use `last_term_info`, which maintains info on all columns that got added
  # and removed from the training data. This is important for skipped steps
  # which might have resulted in columns not being added/removed in the test
  # set.
  info <- object$last_term_info

  # Handle old grouped data.frames
  if (!is.null(attr(info, "vars"))) {
    group_vars <- attr(info, "vars")
    info <- dplyr::ungroup(info)
    info <- dplyr::group_by(info, dplyr::pick(group_vars))
  }

  # Now reduce to only user selected columns
  out_names <- recipes_eval_select(
    terms,
    new_data,
    info,
    check_case_weights = FALSE
  )
  new_data <- new_data[, out_names]

  new_data <- turn_strings_to_factors(object, new_data)

  new_data <- hardhat::recompose(new_data, composition = composition)

  new_data
}

turn_strings_to_factors <- function(object, new_data) {
  ## The levels are not null when no nominal data are present or
  ## if strings_as_factors = FALSE in `prep`
  if (is.null(object$levels)) {
    return(new_data)
  }

  var_levels <- object$levels
  string_names <- intersect(names(var_levels), names(new_data))
  var_levels <- var_levels[string_names]

  not_all_na <- purrr::map_lgl(var_levels, function(x) !all(is.na(x)))
  if (is.null(object$template)) {
    output_factor <- TRUE
  } else {
    output_factor <- purrr::map_lgl(object$template[string_names], is.factor)
  }
  var_levels <- var_levels[not_all_na & output_factor]

  if (length(var_levels) > 0) {
    new_data <- strings2factors(new_data, var_levels)
  }

  new_data
}

#' Print a Recipe
#'
#' @aliases print.recipe
#' @param x A `recipe` object
#' @param form_width The number of characters used to print the variables or
#'   terms in a formula
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @return The original object (invisibly)
#'
#' @export
print.recipe <- function(x, form_width = 30, ...) {
  cli::cli_div(theme = list(.pkg = list("vec-trunc" = Inf, "vec-last" = ", ")))

  cli::cli_h1("Recipe")
  cli::cli_h3("Inputs")

  tab <- table(x$var_info$role, useNA = "ifany")
  tab <- stats::setNames(tab, names(tab))
  names(tab)[is.na(names(tab))] <- "undeclared role"

  roles <- c("outcome", "predictor", "case_weights", "undeclared role")

  tab <- c(
    tab[names(tab) == roles[1]],
    tab[names(tab) == roles[2]],
    tab[names(tab) == roles[3]],
    sort(tab[!names(tab) %in% roles], TRUE),
    tab[names(tab) == roles[4]]
  )

  cli::cli_text("Number of variables by role")

  spaces_needed <- max(nchar(names(tab))) -
    nchar(names(tab)) +
    max(nchar(tab)) -
    nchar(tab)

  cli::cli_verbatim(
    glue("{names(tab)}: {strrep('\ua0', spaces_needed)}{tab}")
  )

  if ("tr_info" %in% names(x)) {
    cli::cli_h3("Training information")
    nmiss <- x$tr_info$nrows - x$tr_info$ncomplete
    nrows <- x$tr_info$nrows

    cli::cli_text(
      "Training data contained {nrows} data points and {cli::no(nmiss)} \\
       incomplete row{?s}."
    )
  }

  if (!is.null(x$steps)) {
    cli::cli_h3("Operations")
  }

  for (step in x$steps) {
    print(step, form_width = form_width)
  }
  cli::cli_end()

  invisible(x)
}

#' Summarize a recipe
#'
#' This function prints the current set of variables/features and some of their
#' characteristics.
#' @aliases summary.recipe
#' @param object A `recipe` object
#' @param original A logical: show the current set of variables or the original
#'   set when the recipe was defined.
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @return A tibble with columns `variable`, `type`, `role`, and `source`. When
#'   `original = TRUE`, an additional column is included named
#'   `required_to_bake` (based on the results of [update_role_requirements()]).
#' @details
#'
#' Note that, until the recipe has been trained, the current and original
#' variables are the same.
#'
#' It is possible for variables to have multiple roles by adding them with
#' [add_role()]. If a variable has multiple roles, it will have more than one
#' row in the summary tibble.
#'
#' @examples
#' rec <- recipe(~., data = USArrests)
#' summary(rec)
#' rec <- step_pca(rec, all_numeric(), num_comp = 3)
#' summary(rec) # still the same since not yet trained
#' rec <- prep(rec, training = USArrests)
#' summary(rec)
#' @export
#' @seealso [recipe()] [prep()]
summary.recipe <- function(object, original = FALSE, ...) {
  if (original) {
    res <- object$var_info
    res <- dplyr::left_join(
      res,
      bake_req_tibble(object),
      by = "role",
      multiple = "all"
    )
  } else {
    res <- object$term_info
  }
  res
}

bake_req_tibble <- function(x) {
  req <- compute_bake_role_requirements(x)
  req <-
    tibble::tibble(role = names(req), required_to_bake = unname(req)) |>
    dplyr::mutate(role = ifelse(role == "NA", NA_character_, role))
  req
}

#' Extract transformed training set
#'
#' @description `r lifecycle::badge('superseded')`
#'
#' As of `recipes` version 0.1.14, **`juice()` is superseded** in favor of
#' `bake(object, new_data = NULL)`.
#'
#' As steps are estimated by `prep`, these operations are applied to the
#' training set. Rather than running [bake()] to duplicate this processing, this
#' function will return variables from the processed training set.
#'
#' @inheritParams bake.recipe
#' @param object A `recipe` object that has been prepared with the option
#'   `retain = TRUE`.
#'
#' @details
#'
#' `juice()` will return the results of a recipe where _all steps_ have been
#' applied to the data, irrespective of the value of the step's `skip` argument.
#'
#' `juice()` can only be used if a recipe was prepped with `retain = TRUE`. This
#' is equivalent to `bake(object, new_data = NULL)` which is the preferred way
#' to extract the transformation of the training data set.
#'
#' @export
#' @seealso [recipe()] [prep()] [bake()]
juice <- function(object, ..., composition = "tibble") {
  if (!fully_trained(object)) {
    cli::cli_abort(
      c(
        "x" = "At least one step has not been trained.",
        "i" = "Please run {.help [{.fun prep}](recipes::prep)}."
      )
    )
  }

  if (!isTRUE(object$retained)) {
    cli::cli_abort(
      "Use {.code retain = TRUE} in {.fun prep} to be able to extract the \\
      training set."
    )
  }

  if (!any(composition == formats)) {
    cli::cli_abort(
      c(
        "x" = "{.arg composition} cannot be {.val {composition}}.",
        "i" = "Allowed values are {.or {.val {formats}}}."
      )
    )
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  # Get user requested columns
  new_data <- object$template
  out_names <- recipes_eval_select(
    terms,
    new_data,
    object$term_info,
    check_case_weights = FALSE
  )
  new_data <- new_data[, out_names]

  new_data <- turn_strings_to_factors(object, new_data)

  new_data <- hardhat::recompose(new_data, composition = composition)

  new_data
}

formats <- c("tibble", "dgCMatrix", "matrix", "data.frame")

# ------------------------------------------------------------------------------

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' @param x A recipe or recipe step
#' @param infra Should recipes itself be included in the result?
#' @return A character vector
#' @name required_pkgs.recipe
#' @keywords internal
#' @export
required_pkgs.recipe <- function(x, infra = TRUE, ...) {
  res <- purrr::map(x$steps, required_pkgs)
  res <- unique(unlist(res))
  if (infra) {
    res <- c("recipes", res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step <- function(x, ...) {
  character(0)
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.check <- function(x, ...) {
  character(0)
}
