#' Crosstable with woe between a (dicotomous) outcome and a predictor variable.
#'
#' Calculates some summaries and the WoE (Weight of Evidence) between a dicotomous outcome and a given predictor variable.
#' Used to biuld the dictionary.
#'
#' @param predictor A atomic vector, usualy with few distinct values.
#' @param outcome The dependent variable. A atomic vector with exactly 2 distinct values.
#' @param odds_offset Default to 1e-6. Offset value to avoid -Inf/Inf from predictor category with only one outcome class. Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with counts, proportions and woe. Warning: woe can possibly be -Inf. Use 'odds_offset' param to avoid that.
#'
#' @examples
#'
#' outc <- rep(c("A", "B"), 20)
#' pred <- sample(c("X", "Y", "Z", "W"), size = 40, replace = TRUE)
#' woe_table(outc, pred)
#'
#' # offset avoid Inf/-Inf
#' woe_table(c(0, 0, 0, 1), c("A", "A", "B", "B"), odds_offset = 1e-6)
#' woe_table(c(0, 0, 0, 1), c("A", "A", "B", "B"), odds_offset = 0)
#'
#' @export
woe_table <- function(predictor, outcome, odds_offset = 1e-6) {
  outcome_original_labels <- unique(outcome)

  if(length(outcome_original_labels) != 2) stop(sprintf("'outcome' must have exactly 2 categories (has %s)", length(outcome_original_labels)))

  woe_expr <- parse(text = sprintf("log((p_%s + odds_offset)/(p_%s + odds_offset))", outcome_original_labels[1], outcome_original_labels[2]))

  woe_tbl <- tibble::tibble(outcome, predictor) %>%
    dplyr::group_by(outcome, predictor) %>%
    dplyr::summarise(n = n()) %>%
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


#' WoE dictionary of a set of predictor variables upon a given dicotomous outcome
#'
#' Builds the woe dictionary of a set of predictor variables upon a given dicotomous outcome.
#' Convenient to make a woe version of the given set of predictor variables and also to allow
#' one to tweak some woe values by hand.
#'
#' @param .data A tbl. The data.frame where the variables come from.
#' @param .outcome unquoted name of the outcome variable.
#' @param ... unquoted names of predictor variables, passed as you would pass variables to \code{dplyr::select()}. This means that you can use all the helpers like \code{starts_with()} and \code{matches()}.
#' @param odds_offset Default to 1e-6. Offset value to avoid -Inf/Inf from predictor category with only one outcome class. Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with summaries and woe for every given predictor variable stacked up.
#'
#' @details You can pass a custom dictionary to \code{step_woe()}. It must have the exactly the same structure of the output of \code{woe_dictionary()}. One easy way to do this is by tweaking an output returned from it.
#'
#' @examples
#'
#' mtcars %>% woe_dictionary(am, cyl, gear:carb)
#'
#' @export
woe_dictionary <- function(.data, .outcome, ..., odds_offset = 1e-6) {
  .outcome <- enquo(.outcome)
  .outcome_vector <- .data %>% dplyr::pull(!!.outcome)

  .data %>%
    dplyr::select(..., -!!.outcome) %>%
    purrr::map(woe_table, outcome = .outcome_vector, odds_offset = odds_offset) %>%
    dplyr::bind_rows(.id = "variable")
}





