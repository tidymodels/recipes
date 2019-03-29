#' Crosstable with woe between a (dicotomous) outcome and a predictor variable.
#'
#' Calculates some summaries and the WoE (Weight of Evidence) between a dicotomous outcome and a given predictor variable.
#' Used to biuld the dictionary.
#'
#' @param outcome The dependent variable. A atomic vector with exactly 2 distinct values.
#' @param predictor A atomic vector, usualy with few distinct values.
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
woe_table <- function(outcome, predictor, odds_offset = 1e-6) {
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



