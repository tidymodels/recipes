#' Create a lagged predictor
#'
#' `step_lag` creates a *specification* of a recipe step that
#'   will add new columns of lagged data. Lagged data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param lag A vector of positive integers. Each specified column will be
#'  lagged for each value in the vector.
#' @param prefix A prefix for generated column names, default to "lag_".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param default Passed to `dplyr::lag`, determines what fills empty rows
#'   left by lagging (defaults to NA).
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#' @export
#' @rdname step_lag
#'
#' @examples
#' n <- 10
#' start <- as.Date('1999/01/01')
#' end <- as.Date('1999/01/10')
#'
#' df <- data.frame(x = runif(n),
#'                  index = 1:n,
#'                  day = seq(start, end, by = "day"))
#'
#' recipe(~ ., data = df) %>%
#'   step_lag(index, day, lag = 2:3) %>%
#'   prep(df) %>%
#'   bake(df)
#'
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()] [step_naomit()]
step_lag <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           lag = 1,
           prefix = "lag_",
           default = NA,
           columns = NULL,
           skip = FALSE,
           id = rand_id("lag")) {
    add_step(
      recipe,
      step_lag_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        lag = lag,
        default = default,
        prefix = prefix,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_lag_new <-
  function(terms, role, trained, lag, default, prefix, columns, skip, id) {
    step(
      subclass = "lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      default = default,
      prefix = prefix,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lag <- function(x, training, info = NULL, ...) {
  step_lag_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lag = x$lag,
    default = x$default,
    prefix = x$prefix,
    columns = terms_select(x$terms, info = info),
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom dplyr select arrange mutate desc
#' @export
bake.step_lag <- function(object, new_data, ...) {

  if (!all(object$lag == as.integer(object$lag)))
    stop("step_lag requires 'lag' argument to be integer valued.",
         call. = FALSE)

  make_call <- function(col, lag_val) {
    call2(
      "lag",
      x = sym(col),
      n = lag_val,
      default = object$default,
      .ns = "dplyr"
    )
  }

  grid <- expand.grid(col = object$columns, lag_val = object$lag,
                      stringsAsFactors = FALSE)
  calls <- purrr::map2(grid$col, grid$lag_val, make_call)
  newname <- paste0(object$prefix, grid$lag_val, "_", grid$col)
  calls <- check_name(calls, new_data, object, newname, TRUE)

  as_tibble(mutate(new_data, !!!calls))
}

print.step_lag <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Lagging ",  sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }
