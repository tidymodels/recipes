#' Create a recipe for preprocessing data
#'
#' A recipe is a description of the steps to be applied to a data set in
#'   order to prepare it for data analysis.
#'
#' @aliases recipe recipe.default recipe.formula
#' @export
recipe <- function(x, ...) {
  UseMethod("recipe")
}

#' @rdname recipe
#' @export
recipe.default <- function(x, ...) {
  rlang::abort("`x` should be a data frame, matrix, or tibble")
}

#' @rdname recipe
#' @param vars A character string of column names corresponding to variables
#'   that will be used in any context (see below)
#' @param roles A character string (the same length of `vars`) that
#'   describes a single role that the variable will take. This value could be
#'   anything but common roles are `"outcome"`, `"predictor"`,
#'   `"case_weight"`, or `"ID"`
#' @param ... Further arguments passed to or from other methods (not currently
#'   used).
#' @param formula A model formula. No in-line functions should be used here
#'  (e.g. `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types of
#'  transformations should be enacted using `step` functions in this package.
#'  Dots are allowed as are simple multivariate outcome terms (i.e. no need for
#'  `cbind`; see Examples). A model formula may not be the best choice for
#'  high-dimensional data with many columns, because of problems with memory.
#' @param x,data A data frame or tibble of the *template* data set
#'   (see below).
#' @return An object of class `recipe` with sub-objects:
#'   \item{var_info}{A tibble containing information about the original data
#'   set columns}
#'   \item{term_info}{A tibble that contains the current set of terms in the
#'   data set. This initially defaults to the same data contained in
#'   `var_info`.}
#'   \item{steps}{A list of `step`  or `check` objects that define the sequence of
#'   preprocessing operations that will be applied to data. The default value is
#'   `NULL`}
#'   \item{template}{A tibble of the data. This is initialized to be the same
#'   as the data given in the `data` argument but can be different after
#'   the recipe is trained.}
#'
#' @includeRmd man/rmd/recipes.Rmd details
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#'
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
#' sp_signed <- rec %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_spatialsign(all_numeric_predictors())
#' sp_signed
#'
#' # ---------------------------------------------------------------------------
#' # formula multivariate example:
#' # no need for `cbind(carbon, hydrogen)` for left-hand side
#'
#' multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#' multi_y <- multi_y %>%
#'   step_center(all_numeric_predictors()) %>%
#'   step_scale(all_numeric_predictors())
#'
#' # ---------------------------------------------------------------------------
#' # example using `update_role` instead of formula:
#' # best choice for high-dimensional data
#'
#' rec <- recipe(biomass_tr) %>%
#'   update_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
#'     new_role = "predictor"
#'   ) %>%
#'   update_role(HHV, new_role = "outcome") %>%
#'   update_role(sample, new_role = "id variable") %>%
#'   update_role(dataset, new_role = "splitting indicator")
#' rec
recipe.data.frame <-
  function(x,
           formula = NULL,
           ...,
           vars = NULL,
           roles = NULL) {
    if (!is.null(formula)) {
      if (!is.null(vars)) {
        rlang::abort(
          paste0(
            "This `vars` specification will be ignored ",
            "when a formula is used"
          )
        )
      }
      if (!is.null(roles)) {
        rlang::abort(
          paste0(
            "This `roles` specification will be ignored ",
            "when a formula is used"
          )
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
      rlang::abort("`vars` should have unique members")
    }
    if (any(!(vars %in% colnames(x)))) {
      rlang::abort("1+ elements of `vars` are not in `x`")
    }

    x <- x[, vars]

    var_info <- tibble(variable = vars)

    ## Check and add roles when available
    if (!is.null(roles)) {
      if (length(roles) != length(vars)) {
        rlang::abort(
          paste0(
            "The number of roles should be the same as the number of ",
            "variables"
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
      too_many_case_weights(case_weights_n)
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
      requirements = requirements
    )
    class(out) <- "recipe"
    out
  }

#' @rdname recipe
#' @export
recipe.formula <- function(formula, data, ...) {
  # check for minus:
  f_funcs <- fun_calls(formula)
  if (any(f_funcs == "-")) {
    rlang::abort("`-` is not allowed in a recipe formula. Use `step_rm()` instead.")
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

form2args <- function(formula, data, ...) {
  if (!is_formula(formula)) {
    formula <- as.formula(formula)
  }

  ## check for in-line formulas
  inline_check(formula)

  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }

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
    too_many_case_weights(case_weights_n)
  }
  roles[case_weights_cols] <- "case_weights"

  ## pass to recipe.default with vars and roles

  list(x = data, vars = vars, roles = roles)
}

inline_check <- function(x) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]

  if (length(funs) > 0) {
    rlang::abort(paste0(
      "No in-line functions should be used here; ",
      "use steps to define baking actions."
    ))
  }

  invisible(x)
}


#' @aliases prep prep.recipe
#' @param x an object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @export
prep <- function(x, ...) {
  UseMethod("prep")
}

#' Estimate a preprocessing recipe
#'
#' For a recipe with at least one preprocessing operation, estimate the required
#'   parameters from a training set that can be later applied to other data
#'   sets.
#' @param training A data frame or tibble that will be used to estimate
#'   parameters for preprocessing.
#' @param fresh A logical indicating whether already trained operation should be
#'   re-trained. If `TRUE`, you should pass in a data set to the argument
#'   `training`.
#' @param verbose A logical that controls whether progress is reported as operations
#'   are executed.
#' @param log_changes A logical for printing a summary for each step regarding
#'  which (if any) columns were added or removed during training.
#' @param retain A logical: should the *preprocessed* training set be saved
#'   into the `template` slot of the recipe after training? This is a good
#'     idea if you want to add more steps later but want to avoid re-training
#'     the existing steps. Also, it is advisable to use `retain = TRUE`
#'     if any steps use the option `skip = FALSE`. **Note** that this can make
#'     the final recipe size large. When `verbose = TRUE`, a message is written
#'     with the approximate object size in memory but may be an underestimate
#'     since it does not take environments into account.
#' @param strings_as_factors A logical: should character columns be converted to
#'   factors? This affects the preprocessed training set (when
#'   `retain = TRUE`) as well as the results of `bake.recipe`.
#' @return A recipe whose step objects have been updated with the required
#'   quantities (e.g. parameter estimates, model objects, etc). Also, the
#'   `term_info` object is likely to be modified as the operations are
#'   executed.
#' @details
#'
#' Given a data set, this function estimates the required quantities and
#' statistics needed by any operations. [prep()] returns an updated recipe
#' with the estimates. If you are using a recipe as a preprocessor for modeling,
#' we **highly recommend** that you use a `workflow()` instead of manually
#' estimating a recipe (see the example in [recipe()]).
#'
#' Note that missing data is handled in the steps; there is no global
#'   `na.rm` option at the recipe level or in [prep()].
#'
#' Also, if a recipe has been trained using [prep()] and then steps
#'   are added, [prep()] will only update the new operations. If
#'   `fresh = TRUE`, all of the operations will be (re)estimated.
#'
#' As the steps are executed, the `training` set is updated. For example,
#'   if the first step is to center the data and the second is to scale the
#'   data, the step for scaling is given the centered data.
#'
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
#'   ) %>%
#'   step_other(Neighborhood, threshold = 0.05) %>%
#'   step_dummy(all_nominal()) %>%
#'   step_interact(~ starts_with("Central_Air"):Year_Built) %>%
#'   step_ns(Longitude, Latitude, deg_free = 5)
#'
#' prep(ames_rec, verbose = TRUE)
#'
#' prep(ames_rec, log_changes = TRUE)
#' @rdname prep
#' @export
prep.recipe <-
  function(x,
           training = NULL,
           fresh = FALSE,
           verbose = FALSE,
           retain = TRUE,
           log_changes = FALSE,
           strings_as_factors = TRUE,
           ...) {
    training <- check_training_set(training, x, fresh)

    tr_data <- train_info(training)

    # Record the original levels for later checking
    orig_lvls <- lapply(training, get_levels)

    if (strings_as_factors) {
      lvls <- lapply(training, get_levels)
      training <- strings2factors(training, lvls)
    } else {
      lvls <- NULL
    }

    # The only way to get the results for skipped steps is to
    # use `retain = TRUE` so issue a warning if this is not the case
    skippers <- map_lgl(x$steps, is_skipable)
    if (any(skippers) & !retain) {
      rlang::warn(
        paste0(
          "Since some operations have `skip = TRUE`, using ",
          "`retain = TRUE` will allow those steps results to ",
          "be accessible."
        )
      )
    }

    if (fresh) {
      x$term_info <- x$var_info
    }

    running_info <- x$term_info %>% mutate(number = 0, skip = FALSE)

    for (i in seq(along.with = x$steps)) {
      needs_tuning <- map_lgl(x$steps[[i]], is_tune)
      if (any(needs_tuning)) {
        arg <- names(needs_tuning)[needs_tuning]
        arg <- paste0("'", arg, "'", collapse = ", ")
        msg <-
          paste0(
            "You cannot `prep()` a tuneable recipe. Argument(s) with `tune()`: ",
            arg,
            ". Do you want to use a tuning function such as `tune_grid()`?"
          )
        rlang::abort(msg)
      }
      note <- paste("oper", i, gsub("_", " ", class(x$steps[[i]])[1]))
      if (!x$steps[[i]]$trained | fresh) {
        if (verbose) {
          cat(note, "[training]", "\n")
        }

        before_nms <- names(training)

        # Compute anything needed for the preprocessing steps
        # then apply it to the current training set
        x$steps[[i]] <-
          prep(x$steps[[i]],
            training = training,
            info = x$term_info
          )
        training <- bake(x$steps[[i]], new_data = training)
        if (!is_tibble(training)) {
          abort("bake() methods should always return tibbles")
        }
        x$term_info <-
          merge_term_info(get_types(training), x$term_info)

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
    # In case a variable was removed, and that removal step used
    # `skip = TRUE`, we need to retain its record so that
    # selectors can be properly used with `bake`. This tibble
    # captures every variable originally in the data or that was
    # created along the way. `number` will be the last step where
    # that variable was available.
    x$last_term_info <-
      running_info %>%
      group_by(variable) %>%
      arrange(desc(number)) %>%
      summarise(
        type = dplyr::first(type),
        role = as.list(unique(unlist(role))),
        source = dplyr::first(source),
        number = dplyr::first(number),
        skip = dplyr::first(skip),
        .groups = "keep"
      )
    x
  }

#' @rdname bake
#' @aliases bake bake.recipe
#' @export
bake <- function(object, ...) {
  UseMethod("bake")
}

#' Apply a trained preprocessing recipe
#'
#' For a recipe with at least one preprocessing operation that has been trained by
#'   [prep()], apply the computations to new data.
#' @param object A trained object such as a [recipe()] with at least
#'   one preprocessing operation.
#' @param new_data A data frame or tibble for whom the preprocessing will be
#'   applied. If `NULL` is given to `new_data`, the pre-processed _training
#'   data_ will be returned (assuming that `prep(retain = TRUE)` was used).
#' @param ... One or more selector functions to choose which variables will be
#'   returned by the function. See [selections()] for more details.
#'   If no selectors are given, the default is to use
#'   [everything()].
#' @param composition Either "tibble", "matrix", "data.frame", or
#'  "dgCMatrix" for the format of the processed data set. Note that
#'  all computations during the baking process are done in a
#'  non-sparse format. Also, note that this argument should be
#'  called **after** any selectors and the selectors should only
#'  resolve to numeric columns (otherwise an error is thrown).
#' @return A tibble, matrix, or sparse matrix that may have different
#'  columns than the original columns in `new_data`.
#' @details [bake()] takes a trained recipe and applies its operations to a
#'  data set to create a design matrix. If you are using a recipe as a
#'  preprocessor for modeling, we **highly recommend** that you use a `workflow()`
#'  instead of manually applying a recipe (see the example in [recipe()]).
#'
#' If the data set is not too large, time can be saved by using the
#'  `retain = TRUE` option of [prep()]. This stores the processed version of the
#'  training set. With this option set, `bake(object, new_data = NULL)`
#'  will return it for free.
#'
#' Also, any steps with `skip = TRUE` will not be applied to the
#'   data when [bake()] is invoked with a data set in `new_data`.
#'   `bake(object, new_data = NULL)` will always have all of the steps applied.
#' @seealso [recipe()], [prep()]
#' @rdname bake
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' ames <- mutate(ames, Sale_Price = log10(Sale_Price))
#'
#' ames_rec <-
#'   recipe(Sale_Price ~ ., data = ames[-(1:6), ]) %>%
#'   step_other(Neighborhood, threshold = 0.05) %>%
#'   step_dummy(all_nominal()) %>%
#'   step_interact(~ starts_with("Central_Air"):Year_Built) %>%
#'   step_ns(Longitude, Latitude, deg_free = 2) %>%
#'   step_zv(all_predictors()) %>%
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
bake.recipe <- function(object, new_data, ..., composition = "tibble") {
  if (rlang::is_missing(new_data)) {
    rlang::abort("'new_data' must be either a data frame or NULL. No value is not allowed.")
  }

  if (is.null(new_data)) {
    return(juice(object, ..., composition = composition))
  }

  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep`.")
  }

  if (!any(composition == formats)) {
    rlang::abort(
      paste0(
        "`composition` should be one of: ",
        paste0("'", formats, "'", collapse = ",")
      )
    )
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  # In case someone used the deprecated `newdata`:
  if (is.null(new_data) || is.null(ncol(new_data))) {
    if (any(names(terms) == "newdata")) {
      rlang::abort("Please use `new_data` instead of `newdata` with `bake`.")
    } else {
      rlang::abort("Please pass a data set to `new_data`.")
    }
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
      abort("bake() methods should always return tibbles")
    }
  }

  # Use `last_term_info`, which maintains info on all columns that got added
  # and removed from the training data. This is important for skipped steps
  # which might have resulted in columns not being added/removed in the test
  # set.
  info <- object$last_term_info

  # Now reduce to only user selected columns
  out_names <- recipes_eval_select(terms, new_data, info,
                                   check_case_weights = FALSE)
  new_data <- new_data[, out_names]

  ## The levels are not null when no nominal data are present or
  ## if strings_as_factors = FALSE in `prep`
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[out_names]
    check_values <-
      vapply(var_levels, function(x) {
        (!all(is.na(x)))
      }, c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0) {
      new_data <- strings2factors(new_data, var_levels)
    }
  }

  if (composition == "dgCMatrix") {
    new_data <- convert_matrix(new_data, sparse = TRUE)
  } else if (composition == "matrix") {
    new_data <- convert_matrix(new_data, sparse = FALSE)
  } else if (composition == "data.frame") {
    new_data <- base::as.data.frame(new_data)
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
  cat("Recipe\n\n")
  cat("Inputs:\n\n")
  no_role <- is.na(x$var_info$role)
  if (any(!no_role)) {
    tab <- as.data.frame(table(x$var_info$role))
    colnames(tab) <- c("role", "#variables")
    print(tab, row.names = FALSE)
    if (any(no_role)) {
      cat("\n ", sum(no_role), "variables with undeclared roles\n")
    }
  } else {
    cat(" ", nrow(x$var_info), "variables (no declared roles)\n")
  }
  if ("tr_info" %in% names(x)) {
    nmiss <- x$tr_info$nrows - x$tr_info$ncomplete
    cat("\nTraining data contained ",
      x$tr_info$nrows,
      " data points and ",
      sep = ""
    )
    if (x$tr_info$nrows == x$tr_info$ncomplete) {
      cat("no missing data.\n")
    } else {
      cat(
        nmiss,
        "incomplete",
        ifelse(nmiss > 1, "rows.", "row."),
        "\n"
      )
    }
  }
  if (!is.null(x$steps)) {
    cat("\nOperations:\n\n")
    for (i in seq_along(x$steps)) {
      print(x$steps[[i]], form_width = form_width)
    }
  }
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
#' @return A tibble with columns `variable`, `type`, `role`,
#'   and `source`. When `original = TRUE`, an additional column is included
#'   named `required_to_bake` (based on the results of
#'   [update_role_requirements()]).
#' @details
#' Note that, until the recipe has been trained,
#' the current and original variables are the same.
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
    res <- dplyr::left_join(res, bake_req_tibble(object), by = "role")
  } else {
    res <- object$term_info
  }
  res
}

bake_req_tibble <- function(x) {
  req <- compute_bake_role_requirements(x)
  req <-
    tibble::tibble(role = names(req), required_to_bake = unname(req)) %>%
    dplyr::mutate(role = ifelse(role == "NA", NA_character_, role))
  req
}



#' Extract transformed training set
#'
#' As of `recipes` version 0.1.14, **`juice()` is superseded** in favor of
#' `bake(object, new_data = NULL)`.
#'
#' As steps are estimated by `prep`, these operations are
#'  applied to the training set. Rather than running [bake()]
#'  to duplicate this processing, this function will return
#'  variables from the processed training set.
#' @inheritParams bake.recipe
#' @param object A `recipe` object that has been prepared
#'   with the option `retain = TRUE`.
#' @details When preparing a recipe, if the training data set is
#'  retained using `retain = TRUE`, there is no need to [bake()] the
#'  recipe to get the preprocessed training set.
#'
#'  `juice()` will return the results of a recipe where _all steps_
#'  have been applied to the data, irrespective of the value of
#'  the step's `skip` argument.
#' @export
#' @seealso [recipe()] [prep()] [bake()]
juice <- function(object, ..., composition = "tibble") {
  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep()`.")
  }

  if (!isTRUE(object$retained)) {
    rlang::abort(paste0(
      "Use `retain = TRUE` in `prep()` to be able ",
      "to extract the training set"
    ))
  }

  if (!any(composition == formats)) {
    rlang::abort(paste0(
      "`composition` should be one of: ",
      paste0("'", formats, "'", collapse = ",")
    ))
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  # Get user requested columns
  new_data <- object$template
  out_names <- recipes_eval_select(terms, new_data, object$term_info,
                                   check_case_weights = FALSE)
  new_data <- new_data[, out_names]

  ## Since most models require factors, do the conversion from character
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[out_names]
    check_values <-
      vapply(var_levels, function(x) {
        (!all(is.na(x)))
      }, c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0) {
      new_data <- strings2factors(new_data, var_levels)
    }
  }

  if (composition == "dgCMatrix") {
    new_data <- convert_matrix(new_data, sparse = TRUE)
  } else if (composition == "matrix") {
    new_data <- convert_matrix(new_data, sparse = FALSE)
  } else if (composition == "data.frame") {
    new_data <- base::as.data.frame(new_data)
  } else if (composition == "tibble") {
    new_data <- tibble::as_tibble(new_data)
  }

  new_data
}

formats <- c("tibble", "dgCMatrix", "matrix", "data.frame")

utils::globalVariables(c("number"))

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
