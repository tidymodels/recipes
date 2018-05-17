#' Create a Recipe for Preprocessing Data
#'
#' A recipe is a description of what steps should be applied to a data set in
#'   order to get it ready for data analysis.
#'
#' @aliases recipe recipe.default recipe.formula
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
recipe <- function(x, ...)
  UseMethod("recipe")

#' @rdname recipe
#' @export
recipe.default <- function(x, ...)
  stop("`x` should be a data frame, matrix, or tibble", call. = FALSE)

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
#'   (e.g. `log(x)`, `x:y`, etc.). These types of transformations
#'   should be enacted using `step` functions in this package. Dots are
#'   allowed as are simple multivariate outcome terms (i.e. no need for
#'   `cbind`; see Examples).
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
#' @details Recipes are alternative methods for creating design matrices and
#'   for preprocessing data.
#'
#' Variables in recipes can have any type of *role* in subsequent analyses
#'   such as: outcome, predictor, case weights, stratification variables, etc.
#'
#' `recipe` objects can be created in several ways. If the analysis only
#'   contains outcomes and predictors, the simplest way to create one is to use
#'   a simple formula (e.g. `y ~ x1 + x2`) that does not contain inline
#'   functions such as `log(x3)`. An example is given below.
#'
#' Alternatively, a `recipe` object can be created by first specifying
#'   which variables in a data set should be used and then sequentially
#'   defining their roles (see the last example).
#'
#' There are two different types of operations that can be
#'  sequentially added to a recipe. **Steps**  can include common
#'  operations like logging a variable, creating dummy variables or
#'  interactions and so on. More computationally complex actions
#'  such as dimension reduction or imputation can also be specified.
#'  **Checks** are operations that conduct specific tests of the
#'  data. When the test is satisfied, the data are returned without
#'  issue or modification. Otherwise, any error is thrown.
#'
#' Once a recipe has been defined, the [prep()] function can be
#'  used to estimate quantities required for the operations using a
#'  data set (a.k.a. the training data). [prep()] returns another
#'  recipe.
#'
#' To apply the recipe to a data set, the [bake()] function is
#'   used in the same manner as `predict` would be for models. This
#'   applies the steps to any data set.
#'
#' Note that the data passed to `recipe` need not be the complete data
#'   that will be used to train the steps (by [prep()]). The recipe
#'   only needs to know the names and types of data that will be used. For
#'   large data sets, `head` could be used to pass the recipe a smaller
#'   data set to save time and memory.
#'
#' @export
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom dplyr full_join
#' @importFrom stats predict
#' @examples
#'
#' ###############################################
#' # simple example:
#' data(biomass)
#'
#' # split data
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' # When only predictors and outcomes, a simplified formula can be used.
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' # Now add preprocessing steps to the recipe.
#'
#' sp_signed <- rec %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_spatialsign(all_predictors())
#' sp_signed
#'
#' # now estimate required parameters
#' sp_signed_trained <- prep(sp_signed, training = biomass_tr)
#' sp_signed_trained
#'
#' # apply the preprocessing to a data set
#' test_set_values <- bake(sp_signed_trained, newdata = biomass_te)
#'
#' # or use pipes for the entire workflow:
#' rec <- biomass_tr %>%
#'   recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_spatialsign(all_predictors())
#'
#' ###############################################
#' # multivariate example
#'
#' # no need for `cbind(carbon, hydrogen)` for left-hand side
#' multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur,
#'                   data = biomass_tr)
#' multi_y <- multi_y %>%
#'   step_center(all_outcomes()) %>%
#'   step_scale(all_predictors())
#'
#' multi_y_trained <- prep(multi_y, training = biomass_tr)
#'
#' results <- bake(multi_y_trained, biomass_te)
#'
#' ###############################################
#' # Creating a recipe manually with different roles
#'
#' rec <- recipe(biomass_tr) %>%
#'   add_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
#'            new_role = "predictor") %>%
#'   add_role(HHV, new_role = "outcome") %>%
#'   add_role(sample, new_role = "id variable") %>%
#'   add_role(dataset, new_role = "splitting indicator")
#' rec
recipe.data.frame <-
  function(x,
           formula = NULL,
           ...,
           vars = NULL,
           roles = NULL) {

    if (!is.null(formula)) {
      if (!is.null(vars))
        stop("This `vars` specification will be ignored when a formula is ",
             "used", call. = FALSE)
      if (!is.null(roles))
        stop("This `roles` specification will be ignored when a formula is ",
             "used", call. = FALSE)

      obj <- recipe.formula(formula, x, ...)
      return(obj)
    }

    if (is.null(vars))
      vars <- colnames(x)

    if (!is_tibble(x))
      x <- as_tibble(x)

    if (any(table(vars) > 1))
      stop("`vars` should have unique members", call. = FALSE)
    if (any(!(vars %in% colnames(x))))
      stop("1+ elements of `vars` are not in `x`", call. = FALSE)

    x <- x[, vars]

    var_info <- tibble(variable = vars)

    ## Check and add roles when available
    if (!is.null(roles)) {
      if (length(roles) != length(vars))
        stop("The number of roles should be the same as the number of ",
             "variables", call. = FALSE)
      var_info$role <- roles
    } else
      var_info$role <- NA

    ## Add types
    var_info <- full_join(get_types(x), var_info, by = "variable")
    var_info$source <- "original"

    ## Return final object of class `recipe`
    out <- list(
      var_info = var_info,
      term_info = var_info,
      steps = NULL,
      template = x,
      levels = NULL,
      retained = NA
    )
    class(out) <- "recipe"
    out
  }

#' @rdname recipe
#' @export
recipe.formula <- function(formula, data, ...) {
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


#' @importFrom stats as.formula
#' @importFrom tibble as_tibble is_tibble

form2args <- function(formula, data, ...) {
  if (!is_formula(formula))
    formula <- as.formula(formula)
  ## check for in-line formulas
  element_check(formula, allowed = NULL)

  if (!is_tibble(data))
    data <- as_tibble(data)

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
  if (length(outcomes) > 0)
    roles <- c(roles, rep("outcome", length(outcomes)))

  ## pass to recipe.default with vars and roles

  list(x = data, vars = vars, roles = roles)
}


#' @aliases prep prep.recipe
#' @param x an object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
prep   <- function(x, ...)
  UseMethod("prep")

#' Train a Data Recipe
#'
#' For a recipe with at least one preprocessing operation, estimate the required
#'   parameters from a training set that can be later applied to other data
#'   sets.
#' @param training A data frame or tibble that will be used to estimate
#'   parameters for preprocessing.
#' @param fresh A logical indicating whether already trained operation should be
#'   re-trained. If `TRUE`, you should pass in a data set to the argument
#'   `training`.
#' @param verbose A logical that controls wether progress is reported as operations
#'   are executed.
#' @param retain A logical: should the *preprocessed* training set be saved
#'   into the `template` slot of the recipe after training? This is a good
#'     idea if you want to add more steps later but want to avoid re-training
#'     the existing steps. Also, it is advisable to use `retain = TRUE`
#'     if any steps use the option `skip = FALSE`.
#' @param stringsAsFactors A logical: should character columns be converted to
#'   factors? This affects the preprocessed training set (when
#'   `retain = TRUE`) as well as the results of `bake.recipe`.
#' @return A recipe whose step objects have been updated with the required
#'   quantities (e.g. parameter estimates, model objects, etc). Also, the
#'   `term_info` object is likely to be modified as the operations are
#'   executed.
#' @details Given a data set, this function estimates the required quantities
#'   and statistics required by any operations.
#'
#' [prep()] returns an updated recipe with the estimates.
#'
#' Note that missing data handling is handled in the steps; there is no global
#'   `na.rm` option at the recipe-level or in [prep()].
#'
#' Also, if a recipe has been trained using [prep()] and then steps
#'   are added, [prep()] will only update the new operations. If
#'   `fresh = TRUE`, all of the operations will be (re)estimated.
#'
#' As the steps are executed, the `training` set is updated. For example,
#'   if the first step is to center the data and the second is to scale the
#'   data, the step for scaling is given the centered data.
#'
#' @rdname prep
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom purrr map_lgl
#' @export
prep.recipe <-
  function(x,
           training = NULL,
           fresh = FALSE,
           verbose = FALSE,
           retain = FALSE,
           stringsAsFactors = TRUE,
           ...) {
    if (is.null(training)) {
      if (fresh)
        stop("A training set must be supplied to the `training` argument ",
             "when `fresh = TRUE`", call. = FALSE)
      training <- x$template
    } else {
      if (!all(x$var_info$variable %in% colnames(training))) {
        stop("Not all variables in the recipe are present in the supplied ",
             "training set", call. = FALSE)
      }
      training <- if (!is_tibble(training))
        as_tibble(training[, x$var_info$variable, drop = FALSE])
      else
        training[, x$var_info$variable]
    }
    tr_data <- train_info(training)
    if (stringsAsFactors) {
      lvls <- lapply(training, get_levels)
      training <- strings2factors(training, lvls)
    } else
      lvls <- NULL

    # The only way to get the results for skipped steps is to
    # use `retain = TRUE` so issue a warning if this is not the case
    skippers <- map_lgl(x$steps, is_skipable)
    if (any(skippers) & !retain)
      warning("Since some operations have `skip = FALSE`, using ",
              "`retain = TRUE` will allow those steps results to ",
              "be accessible.")


    for (i in seq(along = x$steps)) {
      note <-
        paste("oper",  i, gsub("_", " ", class(x$steps[[i]])[1]))
      if (!x$steps[[i]]$trained | fresh) {
        if (verbose)
          cat(note, "[training]", "\n")
        # Compute anything needed for the preprocessing steps
        # then apply it to the current training set

        x$steps[[i]] <-
          prep(x$steps[[i]],
               training = training,
               info = x$term_info)
        training <- bake(x$steps[[i]], newdata = training)
        x$term_info <-
          merge_term_info(get_types(training), x$term_info)

        ## Update the roles and the term source
        ## These next two steps needs to be smarter to find diffs
        if (!is.na(x$steps[[i]]$role))
          x$term_info$role[is.na(x$term_info$role)] <-
          x$steps[[i]]$role

        x$term_info$source[is.na(x$term_info$source)] <- "derived"
      } else {
        if (verbose)
          cat(note, "[pre-trained]\n")
      }
    }

    ## The steps may have changed the data so reassess the levels
    if (stringsAsFactors) {
      lvls <- lapply(training, get_levels)
      check_lvls <- has_lvls(lvls)
      if (!any(check_lvls)) lvls <- NULL
    } else lvls <- NULL

    if (retain)
      x$template <- training

    x$tr_info <- tr_data
    x$levels <- lvls
    x$retained <- retain
    x
  }

#' @rdname bake
#' @aliases bake bake.recipe
#' @author Max Kuhn
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
bake <- function(object, ...)
  UseMethod("bake")

#' Apply a Trained Data Recipe
#'
#' For a recipe with at least one preprocessing operations that has been trained by
#'   [prep.recipe()], apply the computations to new data.
#' @param object A trained object such as a [recipe()] with at least
#'   one preprocessing operation.
#' @param newdata A data frame or tibble for whom the preprocessing will be
#'   applied.
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
#'  columns than the original columns in `newdata`.
#' @details [bake()] takes a trained recipe and applies the
#'   operations to a data set to create a design matrix.
#'
#' If the original data used to train the data are to be
#'  processed, time can be saved by using the `retain = TRUE` option
#'  of [prep()] to avoid duplicating the same operations. With this
#'  option set, [juice()] can be used instead of `bake` with
#'  `newdata` equal to the training set.
#'
#' Also, any steps with `skip = TRUE` will not be applied to the
#'   data when `bake` is invoked. [juice()] will always have all
#'   of the steps applied.
#' @seealso [recipe()], [juice()], [prep()]
#' @rdname bake
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom tidyselect everything
#' @export
bake.recipe <- function(object, newdata, ..., composition = "tibble") {
  if (!fully_trained(object))
    stop("At least one step has not been training. Please ",
         "run `prep`.",
         call. = FALSE)

  if (!any(composition == formats))
    stop("`composition` should be one of: ",
         paste0("'", formats, "'", collapse = ","),
         call. = FALSE)

  if (!is_tibble(newdata)) newdata <- as_tibble(newdata)

  terms <- quos(...)
  if (is_empty(terms))
    terms <- quos(everything())

  ## determine return variables
  keepers <- terms_select(terms = terms, info = object$term_info)

  for (i in seq(along = object$steps)) {
    if (!is_skipable(object$steps[[i]])) {
      newdata <- bake(object$steps[[i]], newdata = newdata)
      if (!is_tibble(newdata))
        newdata <- as_tibble(newdata)
    }
  }

  newdata <- newdata[, names(newdata) %in% keepers]
  ## The levels are not null when no nominal data are present or
  ## if stringsAsFactors = FALSE in `prep`
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[keepers]
    check_values <-
      vapply(var_levels, function(x)
        (!all(is.na(x))), c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0)
      newdata <- strings2factors(newdata, var_levels)
  }

  if (composition == "dgCMatrix") {
    newdata <- convert_matrix(newdata, sparse = TRUE)
  } else if (composition == "matrix") {
    newdata <- convert_matrix(newdata, sparse = FALSE)
  } else if (composition == "data.frame") {
    newdata <- base::as.data.frame(newdata)
  }

  newdata
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
#' @author Max Kuhn
#' @export
print.recipe <- function(x, form_width = 30, ...) {
  cat("Data Recipe\n\n")
  cat("Inputs:\n\n")
  no_role <- is.na(x$var_info$role)
  if (any(!no_role)) {
    tab <- as.data.frame(table(x$var_info$role))
    colnames(tab) <- c("role", "#variables")
    print(tab, row.names = FALSE)
    if (any(no_role)) {
      cat("\n ", sum(no_role), "variables without declared roles\n")
    }
  } else {
    cat(" ", nrow(x$var_info), "variables (no declared roles)\n")
  }
  if ("tr_info" %in% names(x)) {
    nmiss <- x$tr_info$nrows - x$tr_info$ncomplete
    cat("\nTraining data contained ",
        x$tr_info$nrows,
        " data points and ",
        sep = "")
    if (x$tr_info$nrows == x$tr_info$ncomplete)
      cat("no missing data.\n")
    else
      cat(nmiss,
          "incomplete",
          ifelse(nmiss > 1, "rows.", "row."),
          "\n")
  }
  if (!is.null(x$steps)) {
    cat("\nOperations:\n\n")
    for (i in seq_along(x$steps))
      print(x$steps[[i]], form_width = form_width)
  }
  invisible(x)
}

#' Summarize a Recipe
#'
#' This function prints the current set of variables/features and some of their
#'   characteristics.
#' @aliases summary.recipe
#' @param object A `recipe` object
#' @param original A logical: show the current set of variables or the original
#'   set when the recipe was defined.
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @return A tibble with columns `variable`, `type`, `role`,
#'   and `source`.
#' @details Note that, until the recipe has been trained, the current and
#'   original variables are the same.
#' @examples
#' rec <- recipe( ~ ., data = USArrests)
#' summary(rec)
#' rec <- step_pca(rec, all_numeric(), num = 3)
#' summary(rec) # still the same since not yet trained
#' rec <- prep(rec, training = USArrests)
#' summary(rec)
#' @export
#' @seealso [recipe()] [prep.recipe()]
summary.recipe <- function(object, original = FALSE, ...) {
  if (original)
    object$var_info
  else
    object$term_info
}


#' Extract Finalized Training Set
#'
#' As steps are estimated by `prep`, these operations are
#'  applied to the training set. Rather than running `bake`
#'  to duplicate this processing, this function will return
#'  variables from the processed training set.
#' @inheritParams bake.recipe
#' @param object A `recipe` object that has been prepared
#'   with the option `retain = TRUE`.
#' @details When preparing a recipe, if the training data set is
#'  retained using `retain = TRUE`, there is no need to `bake` the
#'  recipe to get the preprocessed training set.
#'
#'  `juice` will return the results of a recipes where _all steps_
#'  have been applied to the data, irrespective of the value of
#'  the step's `skip` argument.
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' sp_signed <- rec %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_spatialsign(all_predictors())
#'
#' sp_signed_trained <- prep(sp_signed, training = biomass_tr, retain = TRUE)
#'
#' tr_values <- bake(sp_signed_trained, newdata = biomass_tr, all_predictors())
#' og_values <- juice(sp_signed_trained, all_predictors())
#'
#' all.equal(tr_values, og_values)
#' @export
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
juice <- function(object, ..., composition = "tibble") {
  if (!fully_trained(object))
    stop("At least one step has not been training. Please ",
         "run `prep`.",
         call. = FALSE)

  if(!isTRUE(object$retained))
    stop("Use `retain = TRUE` in `prep` to be able to extract the training set",
         call. = FALSE)

  if (!any(composition == formats))
    stop("`composition` should be one of: ",
         paste0("'", formats, "'", collapse = ","),
         call. = FALSE)

  terms <- quos(...)
  if (is_empty(terms))
    terms <- quos(everything())
  keepers <- terms_select(terms = terms, info = object$term_info)

  newdata <- object$template[, names(object$template) %in% keepers]

  ## Since most models require factors, do the conversion from character
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[keepers]
    check_values <-
      vapply(var_levels, function(x)
        (!all(is.na(x))), c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0)
      newdata <- strings2factors(newdata, var_levels)
  }

  if (composition == "dgCMatrix") {
    newdata <- convert_matrix(newdata, sparse = TRUE)
  } else if (composition == "matrix") {
    newdata <- convert_matrix(newdata, sparse = FALSE)
  } else if (composition == "data.frame") {
    newdata <- base::as.data.frame(newdata)
  }

  newdata
}

formats <- c("tibble", "dgCMatrix", "matrix", "data.frame")


