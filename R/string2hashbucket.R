#' string2hashbucket Transformation
#'
#' `step_string2hashbucket()` creates a *specification* of a recipe step
#'  that will convert each element of the character column to its hash mod
#'  by \code{num_buckets}. The hash function is deterministic on the content
#'  of the string.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param num_buckets The number of buckets, must be >= 1.
#' @param columns A character string of variable names that will
#'  be (eventually) populated by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `num_buckets`.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_string2hashbucket <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           num_buckets,
           columns = NULL) {
    if (num_buckets < 1) stop("'num_buckets' must be at least 1")
    add_step(
      recipe,
      step_string2hashbucket_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        num_buckets = num_buckets,
        columns = columns
      )
    )
  }

step_string2hashbucket_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           num_buckets = NULL,
           columns = NULL) {
    step(
      subclass = "string2hashbucket",
      terms = terms,
      role = role,
      trained = trained,
      num_buckets = num_buckets,
      columns = columns
    )
  }

#' @export
prep.step_string2hashbucket <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  str_check <-
    vapply(training[, col_names], is.character, logical(1))
  if (any(!str_check))
    stop(
      "The following variables are not character vectors: ",
      paste0("`", names(str_check)[!str_check], "`", collapse = ", "),
      call. = FALSE
    )
  step_string2hashbucket_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_buckets = x$num_buckets,
    columns = col_names
  )
}

#' @export
bake.step_string2hashbucket <- function(object, newdata, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <- getElement(newdata, col_names[i]) %>%
      openssl::md5() %>%
      sapply(function(x) {
        r <- openssl::bignum(x, hex = TRUE) %%
          openssl::bignum(object$num_buckets)
        as.integer(r)
      })
  as_tibble(newdata)
}

print.step_string2hashbucket <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("string2hashbucket transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_string2hashbucket
#' @param x A `step_string2hashbucket` object.
tidy.step_string2hashbucket <- function(x, ...) {
  out <- simple_terms(x, ...)
  out$num_buckets <- x$num_buckets
  out
}
