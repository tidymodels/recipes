#' WoE Transformation
#'
#' `step_woe` creates a *specification* of a
#'  recipe step that will transform nominal data into its numerical
#'  transformation based on weights of evidence against a binary outcome.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the components. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new woe components columns created by the original
#'  variables will be used as predictors in a model.
#' @param outcome bare name of the binary outcome.
#' @param woe_dictionary a tbl. A map of levels and woe values. It must
#' have the same layout than the output returned from [woe_dictionary()].
#' If `NULL`` the function will build a dictionary with those variables
#' passed to \code{...}. See [woe_dictionary()] for details.
#' @param odds_offset Offset value to avoid -Inf/Inf from predictor
#'  category with only one outcome class. Set to 0 to allow Inf/-Inf.
#'  The default is 1e-6.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with the woe dictionary used to map
#'  categories with woe values.
#' @keywords datagen
#' @concept preprocessing woe transformation_methods
#' @export
#' @details
#' WoE is a transformation of a group of variables that produces
#' a new set of features. The formula is
#'
#' \deqn{woe_c = log((P(X = c|Y = 1))/(P(X = c|Y = 0)))}
#'
#' where \eqn{c} goes from 1 to \eqn{C} levels of a given nominal
#' predictor variable \eqn{X}.
#'
#' These components are designed to transform nominal variables into
#' numerical ones with the property that the order and magnitude
#' reflects the association with a binary outcome.  To apply it on
#' numerical predictors, it is advisable to discretize the variables
#' prior to running WoE. Here, each variable will be binarized to
#' have woe associated later. This can achieved by using [step_discretize()].
#'
#' The argument `odds_offset` is an small quantity added to the
#' proportions of 1's and 0's with the goal to avoid log(p/0) or
#' log(0/p) results. The numerical woe versions will have names that
#' begin with `woe_` followed by the respecttive original name of the
#' variables.
#'
#' One can pass a custom `woe_dictionary` tibble to \code{step_woe()}.
#' It must have the same structure of the output from
#' \code{woe_dictionary()} (see examples). If not provided it will be
#' created automatically. The role of this tibble is to store the map
#' between the levels of nominal predictor to its woe values. You may
#' want to tweak this object with the goal to fix the orders between
#' the levels of one given predictor. One easy way to do this is by
#' tweaking an output returned from \code{woe_dictionary()}.
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#' @references Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' @examples
#'
#' data("credit_data")
#'
#' set.seed(111)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[ in_training, ]
#' credit_te <- credit_data[-in_training, ]
#'
#' rec <- recipe(Status ~ ., data = credit_tr) %>%
#'   step_woe(Job, Home, outcome = Status)
#'
#' woe_models <- prep(rec, training = credit_tr)
#'
#' woe_te <- bake(woe_models, new_data = credit_te)
#'
#' head(woe_te)
#' tidy(rec, number = 1)
#' tidy(woe_models, number = 1)
#'
#' # Example of custom dictionary + tweaking
#' # custom woe_dictionary
#' woe_dict_custom <- credit_tr %>% woe_dictionary(Job, Home, outcome = Status)
#' woe_dict_custom[4, "woe"] <- 1.23 #tweak
#'
#' #passing custom dict to step_woe()
#' rec_custom <- recipe(Status ~ ., data = credit_tr) %>%
#'   step_woe(Job, Home, outcome = Status, woe_dictionary = woe_dict_custom) %>%
#'   prep
#'
#' rec_custom_baked <- bake(rec_custom, new_data = credit_te)
#' rec_custom_baked %>% dplyr::filter(woe_Job == 1.23) %>% head
#'
step_woe <- function(recipe,
                     ...,
                     role = "predictor",
                     outcome,
                     trained = FALSE,
                     woe_dictionary = NULL,
                     odds_offset = 1e-6,
                     prefix = "woe",
                     skip = FALSE,
                     id = rand_id("woe")) {
  if(missing(outcome)) stop('argument "outcome" is missing, with no default')

  add_step(
    recipe,
    step_woe_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      outcome = enquo(outcome),
      woe_dictionary = woe_dictionary,
      odds_offset = odds_offset,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

## Initializes a new object
step_woe_new <- function(terms, role, trained, outcome, woe_dictionary, odds_offset, prefix, skip, id) {
  step(
    subclass = "woe",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    woe_dictionary = woe_dictionary,
    odds_offset = odds_offset,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' Crosstable with woe between a binary outcome and a predictor variable.
#'
#' Calculates some summaries and the WoE (Weight of Evidence) between a binary
#' outcome and a given predictor variable. Used to biuld the dictionary.
#'
#' @param predictor A atomic vector, usualy with few distinct values.
#' @param outcome The dependent variable. A atomic vector with exactly 2 distinct values.
#' @param odds_offset Default to 1e-6. Offset value to avoid -Inf/Inf from predictor
#'  category with only one outcome class. Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with counts, proportions and woe.
#'  Warning: woe can possibly be -Inf. Use 'odds_offset' arg to avoid that.
#'
#' @examples
#'
#' outc <- rep(c("A", "B"), 20)
#' pred <- sample(c("X", "Y", "Z", "W"), size = 40, replace = TRUE)
#' woe_table(pred, outc)
#'
#' # offset avoid Inf/-Inf
#' woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), odds_offset = 1e-6)
#' woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), odds_offset = 0)
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#' @references Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' @export
woe_table <- function(predictor, outcome, odds_offset = 1e-6) {
  outcome_original_labels <- unique(outcome)

  if(length(outcome_original_labels) != 2) stop(sprintf("'outcome' must have exactly 2 categories (has %s)", length(outcome_original_labels)))

  woe_expr <- parse(text = sprintf("log((p_%s + odds_offset)/(p_%s + odds_offset))", outcome_original_labels[1], outcome_original_labels[2]))

  woe_tbl <- tibble::tibble(outcome, predictor) %>%
    dplyr::group_by(outcome, predictor) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(predictor) %>%
    dplyr::mutate(n_tot = sum(n)) %>%
    dplyr::group_by(outcome) %>%
    dplyr::mutate(p = n/sum(n)) %>%
    tidyr::gather(summary, value, n, p) %>%
    tidyr::unite(summary_outcome, summary, outcome) %>%
    tidyr::spread(summary_outcome, value, fill = 0) %>%
    dplyr::mutate(
      woe = eval(woe_expr),
      predictor = as.character(predictor)
    )

  return(woe_tbl)
}


#' WoE dictionary of a set of predictor variables upon a given binary outcome
#'
#' Builds the woe dictionary of a set of predictor variables upon a given binary outcome.
#' Convenient to make a woe version of the given set of predictor variables and also to allow
#' one to tweak some woe values by hand.
#'
#' @param .data A tbl. The data.frame where the variables come from.
#' @param outcome bare name of the outcome variable with exactly 2 distinct values.
#' @param ... bare names of predictor variables or selectors accepted by \code{dplyr::select()}.
#' @param odds_offset Default to 1e-6. Offset value to avoid -Inf/Inf from predictor
#'  category with only one outcome class. Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with summaries and woe for every given predictor variable stacked up.
#'
#' @details You can pass a custom dictionary to \code{step_woe()}. It must have the exactly
#' the same structure of the output of [woe_dictionary()]. One easy way to do this
#' is by tweaking an output returned from it.
#'
#' @examples
#'
#' mtcars %>% woe_dictionary(am, cyl, gear:carb)
#'
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#' @references Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' @importFrom rlang !!
#' @export
woe_dictionary <- function(.data, outcome, ..., odds_offset = 1e-6) {
  outcome <- enquo(outcome)
  outcome_vector <- .data %>% dplyr::pull(!!outcome)
  .data %>%
    dplyr::select(..., -!!outcome) %>%
    purrr::map(woe_table, outcome = outcome_vector, odds_offset = odds_offset) %>%
    dplyr::bind_rows(.id = "variable")
}


#' Add WoE in a data.frame
#'
#' A tidyverse friendly way to plug WoE versions of a set of predictor variables against a
#' given binary outcome.
#'
#' @param .data A tbl. The data.frame to plug the new woe version columns.
#' @param outcome unquoted name of the outcome variable.
#' @param ... unquoted names of predictor variables, passed as you would pass variables to
#'  \code{dplyr::select()}. This means that you can use all the helpers like \code{starts_with()}
#'  and \code{matches()}.
#' @param woe_dictionary a tbl. If NULL the function will build a dictionary with those variables
#'  passed to \code{...}. You can pass a custom dictionary too, see [woe_dictionary()]
#'  for details.
#' @param prefix A character string that will be the prefix to the resulting new variables.
#'
#' @return a tibble with the original columns of .data plus the woe columns wanted.
#'
#' @details You can pass a custom dictionary to [add_woe()]. It must have the exactly the same
#'  structure of the output of [woe_dictionary()]. One easy way to do this is to tweak a output
#'  returned from it.
#'
#' @examples
#'
#' mtcars %>% add_woe(am, cyl, gear:carb)
#'
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#' @references Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' @importFrom rlang !!
#' @export
add_woe <- function(.data, outcome, ..., woe_dictionary = NULL, prefix = "woe") {
  if(missing(.data)) stop('argument ".data" is missing, with no default')
  if(missing(outcome)) stop('argument "outcome" is missing, with no default')

  outcome <- rlang::enquo(outcome)
  if(is.null(woe_dictionary)) {
    woe_dictionary <- woe_dictionary(.data, !!outcome, ...)
  } else {
    if(is.null(woe_dictionary$variable)) stop('column "variable" is missing in woe_dictionary.')
    if(is.null(woe_dictionary$predictor)) stop('column "predictor" is missing in woe_dictionary.')
    if(is.null(woe_dictionary$woe)) stop('column "woe" is missing in woe_dictionary.')
  }

  # warns if there is variable with more than 50 levels
  level_counts <- table(woe_dictionary$variable)
  purrr::walk2(
    level_counts,
    names(level_counts),
    ~ if(.x > 50)
      warning("Variable ", .y, " has ", .x, " unique values. Is this expected? In case of numeric variable, see ?step_discretize()."))


  if(missing(...)) {
    dots_vars <- names(.data)
  } else {
    dots_vars <- names(.data %>% select(...))
  }

  output <- woe_dictionary %>%
    dplyr::filter(variable %in% dots_vars) %>%
    dplyr::select(variable, predictor, woe) %>%
    dplyr::group_by(variable) %>%
    tidyr::nest(.key = "woe_table") %>%
    dplyr::mutate(woe_table = purrr::map2(woe_table, variable, ~ purrr::set_names(.x, c(.y, paste0(prefix,"_", .y)))) %>% purrr::set_names(variable))

  output <- purrr::map2(
    output$woe_table,
    output$variable, ~ {
      .data %>%
        dplyr::select(!!.y) %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::left_join(.x, by = .y) %>%
        dplyr::select(starts_with(prefix))
    }) %>%
    dplyr::bind_cols(.data, .) %>%
    tibble::as_tibble()

  output
}


#' @export
prep.step_woe <- function(x, training, info = NULL, ...) {
  outcome_name <- rlang::quo_text(x$outcome)
  col_names <- terms_select(x$terms, info = info)
  col_names <- col_names[!(col_names %in% outcome_name)]
  check_type(training[, col_names], quant = FALSE)

  if(is.null(x$woe_dictionary)) {
    x$woe_dictionary <- woe_dictionary(
      .data = training[, unique(c(outcome_name, col_names))],
      outcome = !!x$outcome
    )
  }

  step_woe_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    woe_dictionary = x$woe_dictionary,
    odds_offset = x$odds_offset,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tibble as_tibble
#' @export
bake.step_woe <- function(object, new_data, ...) {
  woe_dict <- object$woe_dictionary
  woe_vars <- unique(woe_dict$variable)
  new_data <- add_woe(
    .data = new_data,
    outcome = object$outcome,
    woe_dictionary = woe_dict,
    prefix = object$prefix
  )
  new_data <- new_data[, !(colnames(new_data) %in% woe_vars), drop = FALSE]
  as_tibble(new_data)
}

print.step_woe <- function(x, width = max(20, options()$width - 29), ...) {
  cat("WoE version against outcome", rlang::quo_text(x$outcome), "for ")
  printer(unique(x$woe_dictionary$variable), x$terms, x$trained, width = width)
  invisible(x)
}

#' @importFrom utils stack
#' @rdname step_woe
#' @param x A `step_woe` object.
#' @export
tidy.step_woe <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$woe_dictionary
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(variable = term_names,
                  predictor = rlang::na_chr,
                  ntot = rlang::na_int,
                  n_0 = rlang::na_int,
                  n_1 = rlang::na_int,
                  p_0 = rlang::na_dbl,
                  p_1 = rlang::na_dbl,
                  woe = rlang::na_dbl)
  }
  res$id <- x$id
  res
}
