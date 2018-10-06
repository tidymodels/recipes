#' Tidy the Result of a Recipe
#'
#' `tidy` will return a data frame that contains information
#'  regarding a recipe or operation within the recipe (when a `tidy`
#'  method for the operation exists).
#'
#' @name tidy.recipe
#'
#' @param x A `recipe` object (trained or otherwise).
#' @param number An integer or `NA`. If missing, the return
#'  value is a list of the operation in the recipe. If a number is
#'  given, a `tidy` method is executed for that operation in the
#'  recipe (if it exists).
#' @param ... Not currently used.
#' @return A tibble with columns that would vary depending on what
#'  `tidy` method is executed. When `x` is `NA`, a
#'  tibble with columns `number` (the operation iteration),
#'  `operation` (either "step" or "check"),
#'  `type` (the method, e.g. "nzv", "center"), a logical
#'  column called `trained` for whether the operation has been
#'  estimated using `prep`, a logical for `skip`, and a character column `id`. 
#'
#' @examples
#' data(okc)
#'
#' okc_rec <- recipe(~ ., data = okc) %>%
#'   step_other(all_nominal(), threshold = 0.05, other = "another") %>%
#'   step_date(date, features = "dow") %>%
#'   step_center(all_numeric()) %>%
#'   step_dummy(all_nominal()) %>%
#'   check_cols(starts_with("date"), age, height)
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
NULL

#' @rdname tidy.recipe
#' @export
tidy.recipe <- function(x, number = NA, ...) {
  num_oper <- length(x$steps)
  if (num_oper == 0)
    stop("No steps in recipe.", call. = FALSE)
  pattern <- "(^step_)|(^check_)"
  if (is.na(number)) {
    skipped <- vapply(x$steps, function(x) x$skip, logical(1))
    ids <- vapply(x$steps, function(x) x$id, character(1))

    oper_classes <- lapply(x$steps, class)
    oper_classes <- grep("_", unlist(oper_classes), value = TRUE)

    oper <- strsplit(oper_classes, split = "_")
    oper <- vapply(oper, function(x) x[1], character(1))

    oper_types <- gsub(pattern, "", oper_classes)
    is_trained <- vapply(x$steps,
                         function(x) x$trained,
                         logical(1))
    res <- tibble(number = seq_along(x$steps),
                  operation = oper,
                  type = oper_types,
                  trained = is_trained,
                  skip = skipped,
                  id = ids)
  } else {
    if (number > num_oper || length(number) > 1)
      stop("`number` should be a single value between 1 and ",
           num_oper, ".", call. = FALSE)

    res <- tidy(x$steps[[number]], ...)
  }
  res
}

#' @rdname tidy.recipe
#' @export
tidy.step <- function(x, ...) {
  stop("No `tidy` method for a step with classes: ",
       paste0(class(x), collapse = ", "),
       call. = FALSE)
}

#' @rdname tidy.recipe
#' @export
tidy.check <- function(x, ...) {
  stop("No `tidy` method for a check with classes: ",
       paste0(class(x), collapse = ", "),
       call. = FALSE)
}
