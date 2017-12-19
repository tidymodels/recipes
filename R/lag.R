#' Lag a date valued predictor
#'
#' `step_lag` creates a *specification* of a recipe step that
#'   will sort the data into chronological order for each selected
#'   variable and then add a new column of lagged data. Lagged data will
#'   by default include NA values where the lag was induced. These can be
#'   removed with [step_naomit()].
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param prefix A prefix for generated column names, default to "lag_".
#' @param lag A vector of integers. Each specified column will be lagged be
#'   each value in the vector.
#' @param default Passed to `dplyr::lag`, determines what fills empty rows
#'   left by lagging (defaults to NA).
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#' @rdname step_lag
#'
#' @examples
#' n <- 10
#' start <- as.Date('1999/01/01')
#' end <- as.Date('2000/01/01')
#'
#' df <- data.frame(x = runif(n), y = rnorm(n),
#'                  t = sample(seq(start, end, by = "day"), n))
#'
#' baked <- recipe(~ ., data = df) %>%
#'   step_lag(has_type("Date"), lag = 2) %>%
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
           default = NA,
           prefix = "lag_") {
    add_step(
      recipe,
      step_lag_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        lag = lag,
        default = default,
        prefix = prefix
      )
    )
  }

step_lag_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           lag = 1,
           default = NA,
           prefix = "lag_") {
    step(
      subclass = "lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      default = default,
      prefix = prefix)
  }

#' @export
prep.step_lag <- function(x, training, info = NULL, ...) {
  x$columns <- terms_select(x$terms, info = info)
  x$trained <- TRUE
  x
}

#' @import dplyr
#' @export
bake.step_lag <- function(object, newdata, ...) {

  if (!all(purrr::map_lgl(dplyr::select(newdata, object$columns), is.Date)))
    stop("step_lag only works with Date data.", call. = FALSE)

  if (!(is.na(object$default) || is.Date(object$default)))
    stop("step_lag requires 'default' argument to be Date or NA valued.",
         call. = FALSE)

  if (!all(object$lag == as.integer(object$lag)))
    stop("step_lag requires 'lag' argument to be integer valued.",
         call. = FALSE)

  for (col in object$columns) {
    # sort by the feature of interest so we can lag that column
    col <- rlang::sym(col)
    newdata <- arrange(newdata, desc(!!col))
    for (lag_val in object$lag) {
      new_col <- paste0(object$prefix, lag_val, "_", quo_name(col))
      newdata <- mutate(newdata,
                        !!new_col := dplyr::lag(!!col, lag_val, object$default))
    }
  }
  as_tibble(newdata)
}

print.step_lag <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Lagging ",  sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }
