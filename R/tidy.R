#' Tidy the Result of a Recipe
#'
#' `tidy` will return a data frame that contains information
#'  regarding a recipe or step within the recipe (when a `tidy`
#'  method for the step exists).
#'
#' @param x A `recipe` object (trained or otherwise).
#' @param number An integer or `NA`. If missing, the return
#'  value is a list of the steps in the recipe. If a number is
#'  given, a `tidy` method is executed for that step in the
#'  recipe (if it exists).
#' @param ... Not currently used.
#' @return A tibble with columns that would vary depending on what
#'  `tidy` method is executed. When `x` is `NA`, a
#'  tibble with columns `number` (the step iteration),
#'  `type` (the step type, e.g. "nzv", "center"), and a logical
#'  column called `trained` for whether the step has been
#'  estimated using `prep`.
#' @export
#' @examples
#' data(okc)
#'
#' okc_rec <- recipe(~ ., data = okc) %>%
#'   step_other(all_nominal(), threshold = 0.05) %>%
#'   step_date(date, features = "dow") %>%
#'   step_center(all_numeric()) %>%
#'   step_dummy(all_nominal())
#'
#' tidy(okc_rec)
#'
#' tidy(okc_rec, number = 2)
#' tidy(okc_rec, number = 3)
#'
#' okc_rec_trained <- prep(okc_rec, training = okc)
#'
#' tidy(okc_rec_trained)
#' tidy(okc_rec_trained, number = 3)

#' @importFrom broom tidy
tidy.recipe <- function(x, number = NA, ...) {
  num_steps <- length(x$steps)
  if (num_steps == 0)
    stop("No steps in recipe.", call. = FALSE)
  if (is.na(number)) {
    classes <- lapply(x$steps, class)
    classes <- vapply(classes,
                      function(x)
                        grep("^step_", x, value = TRUE)[1],
                      character(1))
    step_types <- gsub("^step_", "", classes)
    is_trained <- vapply(x$steps,
                         function(x) x$trained,
                         logical(1))
    res <- tibble(number = seq_along(step_types),
                  type = step_types,
                  trained = is_trained)
  } else {
    if (number > num_steps || length(number) > 1)
      stop("`number` should be a single value between 1 and ",
           num_steps, ".", call. = FALSE)

    res <- tidy(x$steps[[number]], ...)
  }
  res
}

#' @export
tidy.step <- function(x, ...) {
  stop("No `tidy` method for a step with classes: ",
       paste0(class(x), collapse = ", "),
       call. = FALSE)
}

## Support functions

is_trained <- function(x)
  x$trained

sel2char <- function(x) {
  term_names <- lapply(x, as.character)
  term_names <-
    vapply(term_names,
           function(x) x[-1],
           character(1))
  term_names
}


simple_terms <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res
}

