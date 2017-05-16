

#' @name selections
#' @aliases selections
#' @aliases selection
#' @title Methods for Select Variables in Step Functions
#' @description When selecting variables or model terms in \code{step}
#'   functions, \code{dplyr}-like tools are used. The \emph{selector}
#'   functions can choose variables based on their name, current role, data
#'   type, or any combination of these. The selectors are passed as any other
#'   argument to the step. If the variables are explicitly stated in the step
#'   function, this might be similar to:
#'
#' \preformatted{
#'   recipe( ~ ., data = USArrests) \%>\%
#'     step_pca(Murder, Assault, UrbanPop, Rape, num = 3)
#' }
#'
#' The first four arguments indicate which variables should be used in the
#'   PCA while the last argument is a specific argument to
#'   \code{\link{step_pca}}.
#'
#' Note that:
#'
#'   \enumerate{
#'     \item The selector arguments should not contain functions beyond those
#'       supported (see below).
#'     \item These arguments are not processed until the \code{learn} function
#'       for the step is executed.
#'     \item The \code{dplyr}-like syntax allows for negative sings to exclude
#'       variables (e.g. \code{-Murder}) and the set of selectors will
#'       processed in order.
#'     \item A leading exclusion in these arguments (e.g. \code{-Murder}) has
#'       the effect of adding all variables to the list except the excluded
#'       variable(s).
#'   }
#'
#' Also, select helpers from the \code{dplyr} package can also be used:
#'   \code{\link[dplyr]{starts_with}}, \code{\link[dplyr]{ends_with}},
#'   \code{\link[dplyr]{contains}}, \code{\link[dplyr]{matches}},
#'   \code{\link[dplyr]{num_range}}, and \code{\link[dplyr]{everything}}.
#'   For example:
#'
#' \preformatted{
#'   recipe(Species ~ ., data = iris) \%>\%
#'     step_center(starts_with("Sepal"), -contains("Width"))
#' }
#'
#' would only select \code{Sepal.Length}
#'
#' \bold{Inline} functions that specify computations, such as \code{log(x)},
#'   should not be used in selectors and will produce an error. A list of
#'   allowed selector functions is below.
#'
#' Columns of the design matrix that may not exist when the step is coded can
#'   also be selected. For example, when using \code{step_pca}, the number of
#'   columns created by feature extraction may not be known when subsequent
#'   steps are defined. In this case, using \code{matches("^PC")} will select
#'   all of the columns whose names start with "PC" \emph{once those columns
#'   are created}.
#'
#' There are sets of functions that can be used to select variables based on
#'   their role or type: \code{\link{has_role}} and \code{\link{has_type}}.
#'   For convenience, there are also functions that are more specific:
#'   \code{\link{all_numeric}}, \code{\link{all_nominal}},
#'   \code{\link{all_predictors}}, and \code{\link{all_outcomes}}. These can
#'   be used in conjunction with the previous functions described for
#'   selecting variables using their names:
#'
#' \preformatted{
#'   data(biomass)
#'   recipe(HHV ~ ., data = biomass) \%>\%
#'     step_center(all_numeric(), -all_outcomes())
#' }
#'
#' This results in all the numeric predictors: carbon, hydrogen, oxygen,
#'   nitrogen, and sulfur.
#'
#' If a role for a variable has not been defined, it will never be selected
#'   using role-specific selectors.
#'
#' All steps use these techniques to define variables for steps
#'   \emph{except one}: \code{\link{step_interact}} requires traditional model
#'   formula representations of the interactions and takes a single formula
#'   as the argument to select the variables.
#'
#' The complete list of allowable functions in steps:
#'
#'   \itemize{
#'     \item \bold{By name}: \code{\link[dplyr]{starts_with}},
#'       \code{\link[dplyr]{ends_with}}, \code{\link[dplyr]{contains}},
#'       \code{\link[dplyr]{matches}}, \code{\link[dplyr]{num_range}}, and
#'       \code{\link[dplyr]{everything}}
#'     \item \bold{By role}: \code{\link{has_role}},
#'       \code{\link{all_predictors}}, and \code{\link{all_outcomes}}
#'     \item \bold{By type}: \code{\link{has_type}}, \code{\link{all_numeric}},
#'       and \code{\link{all_nominal}}
#'   }
NULL

## These are the allowable functions for formulas in the the `terms` arguments
## to the steps or to `recipes.formula`.
name_selectors <- c("starts_with",
                    "ends_with",
                    "contains",
                    "matches",
                    "num_range",
                    "everything",
                    "_F")

role_selectors <-
  c("has_role", "all_predictors", "all_outcomes", "_F")

type_selectors <- c("has_type", "all_numeric", "all_nominal", "_F")

selectors <-
  unique(c(name_selectors, role_selectors, type_selectors))

parse_terms_formula <- function(f, info) {
  var_vals <- info$variable
  role_vals <- info$role
  type_vals <- info$type
  
  ## split the terms up using +/- as seperators
  f_info <- f_elements(f)
  elmts <- f_info$terms
  elmts_sign <- f_info$signs
  
  ## Look for inappropriate functions in elements
  check_elements(f)
  
  ## determine if there is a selector involved
  has_func <- has_selector(elmts)
  
  indices <- vector(mode = "list", length = length(elmts) - 1)
  
  for (i in seq_along(elmts)[-1]) {
    if (has_func[i - 1]) {
      cll <- as.call(elmts[[i]])
      cll <- add_arg(cll)
      indices[[i - 1]] <- eval(cll)
    } else {
      indices[[i - 1]] <- which(as.character(elmts[[i]]) == var_vals)
    }
    if (elmts_sign[i - 1] == "-")
      indices[[i - 1]] <- -indices[[i - 1]]
  }
  indices <- unlist(unique(indices))
  
  ## add/subtract based on sign of elements
  if (!all(sign(indices) == 1)) {
    pos <- indices[indices > 0]
    neg <- indices[indices < 0]
    indices <- pos[!(pos %in% abs(neg))]
  }
  
  if (length(indices) == 0)
    stop("No columns were selected by the `terms` formula for this step.",
         call. = FALSE)
  
  var_vals[indices]
}

## Get the components of the formula split by +/-. The
## function also returns the sign
f_elements <- function(x) {
  trms_obj <- terms(x)
  ## Their order will change here (minus at the end)
  clls <- attr(trms_obj, "variables")
  ## Any formula element with a minus prefix will not
  ## have an colname in the `factor` attribute of the
  ## terms object. We will check these against the
  ## list of calls
  tmp <- colnames(attr(trms_obj, "factors"))
  kept <- vector(mode = "list", length = length(tmp))
  for (j in seq_along(tmp))
    kept[[j]] <- as.name(tmp[j])
  
  term_signs <- rep("", length(clls) - 1)
  for (i in seq_along(term_signs)) {
    ## Check to see if the elements are in the `factors`
    ## part of `terms` and these will have a + sign
    retained <- any(unlist(lapply(kept,
                                  function(x, y)
                                    any(y == x),
                                  y = clls[[i + 1]])))
    term_signs[i] <- if (retained)
      "+"
    else
      "-"
  }
  list(terms  = clls, signs = term_signs)
}

## This adds the appropriate argument based on whether the call is for
## a variable name, role, or data type.
add_arg <- function(cl) {
  func <- fun_calls(cl)
  if (func %in% name_selectors) {
    cl$vars <- quote(var_vals)
  } else {
    if (func %in% role_selectors) {
      cl$roles <- quote(role_vals)
    } else
      cl$types <- quote(type_vals)
  }
  cl
}

## This flags formulas that are not allowed. When called from `recipe.formula`
## `allowed` is NULL.
check_elements <- function(x, allowed = selectors) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]
  if (!is.null(allowed)) {
    # when called from a step
    not_good <- funs[!(funs %in% allowed)]
    if (length(not_good) > 0)
      stop(
        "Not all functions are allowed in step function selectors (e.g. ",
        paste0("`", not_good, "`", collapse = ", "),
        "). See ?selections.",
        call. = FALSE
      )
  } else {
    # when called from formula.recipe
    if (length(funs) > 0)
      stop(
        "No in-line functions should be used here; use steps to define ",
        "processing actions", call. = FALSE
      )
  }
  invisible(NULL)
}

has_selector <- function(x, allowed = selectors) {
  res <- rep(NA, length(x) - 1)
  for (i in 2:length(x))
    res[[i - 1]] <- isTRUE(fun_calls(x[[i]]) %in% allowed)
  res
}

#' Select Terms in a Step Function.
#'
#' This function processes the step function selectors and might be useful
#'   when creating custom steps.
#'
#' @param info A tibble with columns \code{variable}, \code{type}, \code{role},
#'   and \code{source} that represent the current state of the data. The
#'   function \code{\link{summary.recipe}} can be used to get this information
#'   from a recipe.
#' @param args A list of formulas whose right-hand side contains quoted
#'   expressions. See \code{\link[rlang]{quos}} for examples.
#' @keywords datagen
#' @concept preprocessing
#' @return A character string of column names or an error of there are no
#'   selectors or if no variables are selected.
#' @seealso \code{\link{recipe}} \code{\link{summary.recipe}}
#'   \code{\link{learn.recipe}}
#' @importFrom purrr map_lgl map_if map_chr map
#' @importFrom rlang names2
#' @importFrom dplyr combine_indices set_current_vars quo_is_select_helper
#' @export
#' @examples
#' library(rlang)
#' data(okc)
#' rec <- recipe(~ ., data = okc)
#' info <- summary(rec)
#' select_terms(info = info, quos(all_predictors()))
select_terms <- function(info, args) {
  ## This is a modified version of dplyr:::select_vars
  
  vars <- info$variable
  roles <- info$role
  types <- info$type
  
  if (is_empty(args))
    stop("At least one selector should be used")
  
  ## check arguments against whitelist
  lapply(args, check_elements)
  
  # Set current_info so available to helpers
  old <- set_current_info(info)
  on.exit(set_current_info(old), add = TRUE)
  # Set current_vars so available to select_helpers
  old <- set_current_vars(vars)
  on.exit(set_current_vars(old), add = TRUE)
  
  # Map variable names to their positions: this keeps integer semantics
  names_list <- set_names(as.list(seq_along(vars)), vars)
  
  # if the first selector is exclusive (negative), start with all columns
  first <- f_rhs(args[[1]])
  initial_case <-
    if (is_negated(first))
      list(seq_along(vars))
  else
    integer(0)
  
  # Evaluate symbols in an environment where columns are bound, but
  # not calls (select helpers are scoped in the calling environment)
  is_helper <- map_lgl(args, quo_is_select_helper)
  ind_list <- map_if(args, is_helper, eval_tidy)
  ind_list <- map_if(ind_list, !is_helper, eval_tidy, names_list)
  
  ind_list <- c(initial_case, ind_list)
  names(ind_list) <- c(names2(initial_case), names2(args))
  
  is_numeric <- map_lgl(ind_list, is.numeric)
  if (any(!is_numeric))
    stop("No variables or terms were selected.", call. = FALSE)
  
  incl <- combine_indices(vars, ind_list)
  
  # Include/exclude specified variables
  sel <- set_names(vars[incl], names(incl))
  
  # Ensure all output vars named
  if (is_empty(sel)) {
    stop("No variables or terms were selected.", call. = FALSE)
  } else {
    unnamed <- names2(sel) == ""
    names(sel)[unnamed] <- sel[unnamed]
  }
  
  unname(sel)
}

#' Role Selection
#'
#' \code{has_role}, \code{all_predictors}, and \code{all_outcomes} can be used
#'   to select variables in a formula that have certain roles. Similarly,
#'   \code{has_type}, \code{all_numeric}, and \code{all_nominal} are used to
#'   select columns based on their data type. See \code{\link{selections}} for
#'   more details. \code{current_info} is an internal function that is
#'   unlikely to help users while the others have limited utility outside of
#'   step function arguments.
#'
#' @param match A single character string for the query. Exact matching is
#'   used (i.e. regular expressions won't work).
#' @param roles A character string of roles for the current set of terms.
#' @param types A character string of roles for the current set of data types
#' @return Selector functions return an integer vector while
#'   \code{current_info} returns an environment with vectors \code{vars},
#'   \code{roles}, and \code{types}.
#' @keywords datagen
#' @examples
#' data(biomass)
#'
#' rec <- recipe(biomass) %>%
#'   add_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
#'            new_role = "predictor") %>%
#'   add_role(HHV, new_role = "outcome") %>%
#'   add_role(sample, new_role = "id variable") %>%
#'   add_role(dataset, new_role = "splitting indicator")
#' recipe_info <- summary(rec)
#' recipe_info
#'
#' has_role("id variable", roles = recipe_info$role)
#' all_outcomes(roles = recipe_info$role)
#' @export

has_role <-
  function(match = "predictor",
           roles = current_info()$roles)
    which(roles %in% match)

#' @export
#' @rdname has_role
#' @inheritParams has_role
all_predictors <- function(roles = current_info()$roles)
  has_role("predictor", roles = roles)

#' @export
#' @rdname has_role
#' @inheritParams has_role
all_outcomes <- function(roles = current_info()$roles)
  has_role("outcome", roles = roles)

#' @export
#' @rdname has_role
#' @inheritParams has_role
has_type <-
  function(match = "numeric",
           types = current_info()$types)
    which(types %in% match)

#' @export
#' @rdname has_role
#' @inheritParams has_role
all_numeric <- function(types = current_info()$types)
  has_type("numeric", types = types)

#' @export
#' @rdname has_role
#' @inheritParams has_role
all_nominal <- function(types = current_info()$types)
  has_type("nominal", types = types)

## functions to get current variable info for selectors modeled after
## dplyr versions

#' @import rlang
cur_info_env <- child_env(env_parent(env))

set_current_info <- function(x) {
  # stopifnot(!is.environment(x))
  old <- cur_info_env
  cur_info_env$vars <- x$variable
  cur_info_env$roles <- x$role
  cur_info_env$types <- x$type
  
  invisible(old)
}

#' @export
#' @rdname has_role
current_info <- function() {
  cur_info_env %||% stop("Variable context not set", call. = FALSE)
}
