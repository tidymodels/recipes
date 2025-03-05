#' Remove steps from recipe
#'
#' `ignore_step` will return a recipe without steps specified by the `number` or
#' `id` argument.
#' 
#' @param x A `recipe` object.
#' @param number An integer vector, Denoting the positions of the steps that 
#'   should be removed.
#' @param id A character string. Denoting the `id` of the steps that should be
#'   removed.
#' 
#' @details
#' `number` or `id` must be specified. Specifying neither or both will result
#' in a error.
#' 
#' @return a `recipe` object.
#'
#' @examples
#' rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_dummy(all_nominal_predictors()) %>%
#'   step_impute_mean(all_numeric_predictors()) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_pca(all_numeric_predictors(), id = "PCA")
#' 
#' ignore_step(rec, number = 1)
#' 
#' ignore_step(rec, number = 1:2)
#' 
#' ignore_step(rec, id = "PCA")
#' @export
ignore_step <- function(x, number, id) {
  if (any(map_lgl(x$steps, is_trained))) {
    cli::cli_abort(
      "{.arg x} must not contain any trained steps."
    )
  }
  
  n_steps <- length(x$steps)

  if (n_steps == 0) {
    cli::cli_abort("{.arg x} doesn't contain any steps to remove.")
  }

  arg <- rlang::check_exclusive(number, id)
  
  if (arg == "number") {
    if (any(number < 1 | number > n_steps)) {
      offenders <- number[number < 1 | number > n_steps]
      cli::cli_abort(
        "{.arg number} must only contain values between 1 and {n_steps}. \\
        Not {offenders}."
      )
    }
  } else {
    step_ids <- vapply(x$steps, function(x) x$id, character(1))
    if (!(id %in% step_ids)) {
      cli::cli_abort(
        "Supplied {.arg id} ({.val {id}}) not found in the recipe."
      )
    }
    number <- which(id == step_ids)
  }
  
  x$steps <- x$steps[-number]

  if (length(x$steps) == 0) {
    x["steps"] <- list(NULL)
  }  
  
  x
}
