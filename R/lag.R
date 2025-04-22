#' Create a lagged predictor
#'
#' `step_lag()` creates a *specification* of a recipe step that will add new
#' columns of lagged data. Lagged data will by default include NA values where
#' the lag was induced. These can be removed with [step_naomit()], or you may
#' specify an alternative filler value with the `default` argument.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param lag A vector of positive integers. Each specified column will be
#'   lagged for each value in the vector.
#' @param prefix A prefix for generated column names, default to `"lag_"`.
#' @param default Passed to [dplyr::lag()], determines what fills empty rows
#'   left by lagging (defaults to NA).
#' @template step-return
#' @details
#'
#' The step assumes that the data are already _in the proper sequential order_
#' for lagging.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-preserve
#'
#' @template case-weights-not-supported
#'
#' @family row operation steps
#' @export
#' @rdname step_lag
#'
#' @examples
#' n <- 10
#' start <- as.Date("1999/01/01")
#' end <- as.Date("1999/01/10")
#'
#' df <- data.frame(
#'   x = runif(n),
#'   index = 1:n,
#'   day = seq(start, end, by = "day")
#' )
#'
#' recipe(~., data = df) |>
#'   step_lag(index, day, lag = 2:3) |>
#'   prep(df) |>
#'   bake(df)
step_lag <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    lag = 1,
    prefix = "lag_",
    default = NA,
    columns = NULL,
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("lag")
  ) {
    add_step(
      recipe,
      step_lag_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        lag = lag,
        default = default,
        prefix = prefix,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_lag_new <-
  function(
    terms,
    role,
    trained,
    lag,
    default,
    prefix,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      default = default,
      prefix = prefix,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lag <- function(x, training, info = NULL, ...) {
  if (!all(x$lag == as.integer(x$lag))) {
    cli::cli_abort(
      "{.arg lag} argument must be integer-valued, \\
      not {.obj_type_friendly {lag}}."
    )
  }
  check_string(x$prefix, arg = "prefix")

  step_lag_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lag = x$lag,
    default = x$default,
    prefix = x$prefix,
    columns = recipes_eval_select(x$terms, training, info),
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lag <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_values <- lapply(
      object$lag,
      function(x) {
        if (sparsevctrs::is_sparse_vector(new_data[[col_name]])) {
          sparsevctrs::sparse_lag(
            new_data[[col_name]],
            x,
            default = object$default
          )
        } else {
          dplyr::lag(new_data[[col_name]], x, default = object$default)
        }
      }
    )

    new_names <- glue::glue("{object$prefix}{object$lag}_{col_name}")
    names(new_values) <- new_names

    new_values <- tibble::new_tibble(new_values)
    new_values <- check_name(new_values, new_data, object, new_names)
    new_data <- vctrs::vec_cbind(new_data, new_values, .name_repair = "minimal")
  }

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_lag <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Lagging "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_lag <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}

#' @export
.recipes_preserve_sparsity.step_lag <- function(x, ...) {
  TRUE
}
