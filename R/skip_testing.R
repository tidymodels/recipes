#' Check recipe for potentially problematic steps
#'
#' When using a recipe on new data where the outcome is not known, recipe
#' steps or checks may fail if the outcome column(s) are not available. To
#' avoid this, steps/checks can be configured to be skipped when `bake()` is
#' run. `check_skips_for_new_data()` can be used to identify if this is the
#' case and, if so, issue a warning that lists the offending step/check(s).
#' @param x A recipe.
#' @return NULL, invisibly. A warning is issued if a problem is detected.
#' @details
#' For un-prepped recipes, there will be false negatives since some selectors
#' cannot be specifically checked until they are resolved (e.g. `all_numeric()`).
#' @examples
#'
#' base_rec <- recipe(Sepal.Width ~ ., data = iris)
#'
#' # Should fail (unfortunately)
#' rec_1 <-
#'   base_rec %>%
#'   step_normalize(all_numeric())
#'
#' check_skips_for_new_data(rec_1)
#' check_skips_for_new_data(prep(rec_1))
#'
#' rec_2 <-
#'   base_rec %>%
#'   step_normalize(Sepal.Width)
#'
#' check_skips_for_new_data(rec_2)
#' check_skips_for_new_data(prep(rec_2))
#'
#' rec_3 <-
#'   base_rec %>%
#'   step_normalize(contains("Sepal.Width"))
#'
#' check_skips_for_new_data(rec_3)
#' check_skips_for_new_data(prep(rec_3))
#'
#' rec_4 <-
#'   base_rec %>%
#'   step_normalize(contains('Sepal.Width'))
#'
#' check_skips_for_new_data(rec_4)
#' check_skips_for_new_data(prep(rec_4))
#'
#' rec_5 <-
#'   base_rec %>%
#'   step_normalize(all_outcomes())
#'
#' check_skips_for_new_data(rec_5)
#' check_skips_for_new_data(prep(rec_5))

check_skips_for_new_data <- function(x) {
  no_skip <- purrr::map_lgl(x$steps, ~ !.x$skip)
  if (all(!no_skip)) {
    return(invisible(NULL))
  }
  stps <- x$steps[no_skip]
  cls <- purrr::map_chr(stps, ~ class(.x)[1])
  trms <- get_term_values(stps)
  pattern <- make_regex(x)
  has_y <-  purrr::map_lgl(trms, ~ any(grepl(pattern, .x)))
  if (any(has_y)) {
    stp_names <- paste0(cls[has_y], "()", collapse = ", ")
    msg <-
      paste0("There are steps in the recipe that use the outcome but have ",
             "`skip = FALSE`. These will likely fail when new data are ",
             "processed by the recipe. See https://bit.ly/skip-steps . ",
             "The step(s): ", stp_names)
    rlang::warn(msg)
  }
  invisible(NULL)
}


make_regex <- function(x) {
  rec_info <- summary(x)
  y_names <- rec_info$variable[rec_info$role == "outcome"]
  y_pat_1 <- paste0("('", y_names, "')", collapse = "|")
  y_pat_2 <- paste0("(\"", y_names, "\")", collapse = "|")
  y_pat_3 <- paste0("(", c(y_names, "all_outcomes", "everything"), ")", collapse = "|")
  pattern <- paste(y_pat_1, y_pat_2, y_pat_3, sep = "|")
  pattern
}

get_term_values <- function(x) {
  trms <- map(x, deparse_terms)
  trms
}

deparse_terms <- function(x) {
  if (!any(names(x) == "terms")) {
    return(character(0))
  }
  purrr::map_chr(x$terms, ~ rlang::get_expr(.x) %>% rlang::expr_text())
}
